namespace SwaggerProvider

open System
open System.Linq

open Microsoft.OpenApi
open Microsoft.OpenApi.Readers

open SwaggerProvider.Internal.Schema

module MicrosoftOpenApiAdapter =
    open System.Collections.Generic

    let rec mapSchema (schema: Models.OpenApiSchema) : SchemaObject =

        let (|IsEnum|_|) (obj: Models.OpenApiSchema) =
            // Parse `enum` - http://json-schema.org/latest/json-schema-validation.html#anchor76
            obj.Enum
            |> Option.ofObj
            |> Option.map (fun enum ->
                enum
                |> Seq.choose (fun enumValue ->
                    match enumValue with
                    | :? Any.OpenApiPrimitive<string> as primitive -> Some primitive.Value
                    | _ -> None
                )
                |> Seq.toArray
            )


        let (|IsRef|_|) (obj: Models.OpenApiSchema) =
            obj.Reference
            |> Option.ofObj
            |> Option.map (fun ref -> ref.ReferenceV2)

        let (|IsArray|_|) (obj: Models.OpenApiSchema) =
        // Parse Arrays - http://json-schema.org/latest/json-schema-validation.html#anchor36
        // TODO: `items` may be an array, `additionalItems` may be filled
            if obj.Type = "array" then
                Some (mapSchema obj.Items)
            else
                None

        let (|IsPrimitive|_|) (obj: Models.OpenApiSchema) =
                
            let format = obj.Format

            match obj.Type with
            | "boolean" -> Some Boolean
            | "integer" when format = "int32" -> Some Int32
            | "integer" -> Some Int64
            | "number" when format = "float" -> Some Float
            | "number" when format = "int32" -> Some Int32
            | "number" when format = "int64" -> Some Int64
            | "number" -> Some Double
            | "string" when format = "date" -> Some Date
            | "string" when format = "date-time" -> Some DateTime
            | "string" when format = "byte" -> Some <| Array Byte
            | "string" -> Some String
            | "file" -> Some File
            | _ -> None

        let (|IsObject|_|) (obj: Models.OpenApiSchema) =
            // TODO: Parse Objects
            obj.Properties
            |> Option.ofObj
            |> Option.map (fun properties ->
                properties
                |> Seq.map (mapDefinitionProperty obj)
                |> Seq.toArray
            )

        let (|IsDict|_|) (obj: Models.OpenApiSchema) =
            // Parse Object that represent Dictionary
            if obj.Type = "object" then
                Some (mapSchema obj.AdditionalProperties)
            else
                None

        let (|IsAllOf|_|) (obj: Models.OpenApiSchema) =
            // Identify composition element 'allOf'
            obj.AllOf
            |> Option.ofObj
            |> Option.bind (fun allOf ->
                if allOf.Count = 0 then None
                else 
                    allOf
                    |> Seq.map (fun schema -> 
                        schema.Properties
                        |> Seq.map (fun prop ->
                            mapDefinitionProperty schema prop
                        )
                    )
                    |> Seq.concat
                    |> Seq.toArray
                    |> Some
            )
                
        let (|IsComposition|_|) (obj: Models.OpenApiSchema) =
            // Models with Object Composition
            match obj with
            | IsAllOf allOf -> Some(allOf)
            | _ -> None

        let (|IsPolymorphism|_|) (obj: Models.OpenApiSchema) =
            // Models with Polymorphism Support
            obj.Discriminator
            |> Option.ofObj
                

        match schema with
        | IsEnum cases     -> Enum cases
        //| IsRef ref        -> Reference ref
        | IsArray itemTy   -> Array itemTy
        | IsPrimitive ty   -> ty
        | IsDict itemTy    -> SchemaObject.Dictionary itemTy
        | IsObject objProps & IsComposition compProps ->
            Object <| Array.append compProps objProps
        | IsObject props   -> Object props
        | IsComposition props -> Object props
        //| IsWrapper ty -> ty
        | IsPolymorphism _ ->
            failwith "Models with Polymorphism Support is not supported yet. If you see this error please report it on GitHub (https://github.com/fsprojects/SwaggerProvider/issues) with schema example."
        | _ -> Object [||] // Default type when parsers could not determine the type based ob schema.
                            // Example of schema : {}

    and mapDefinitionProperty schema prop : DefinitionProperty =
        {
            DefinitionProperty.Name = prop.Key
            DefinitionProperty.Description = prop.Value.Description
            DefinitionProperty.IsRequired = schema.Required.Contains(prop.Key)
            DefinitionProperty.Type = mapSchema prop.Value
        }

    let parseInfo (infoObject: Models.OpenApiInfo) : InfoObject =
        {
            Description = infoObject.Description;
            InfoObject.Title = infoObject.Title;
            InfoObject.Version = infoObject.Version
        }

    let parsePaths (apiPaths: Models.OpenApiPaths) : OperationObject array = 

        let mapOperationType = function 
            | Models.OperationType.Get -> OperationType.Get
            | Models.OperationType.Post -> OperationType.Post
            | Models.OperationType.Put -> OperationType.Put
            | Models.OperationType.Delete -> OperationType.Delete


        let mapParameterLocation = function 
            | Models.ParameterLocation.Cookie -> ParameterObjectLocation.Header
            | Models.ParameterLocation.Header -> ParameterObjectLocation.Header
            | Models.ParameterLocation.Path -> ParameterObjectLocation.Path
            | Models.ParameterLocation.Query -> ParameterObjectLocation.Query

        let mapOperationParameters (parameters: IList<Models.OpenApiParameter>) : ParameterObject array =
            parameters
            |> Seq.map (fun param ->
                {
                    ParameterObject.Name = param.Name
                    ParameterObject.In = mapParameterLocation param.In.Value
                    ParameterObject.Description = param.Description
                    ParameterObject.Required = param.Required
                    ParameterObject.CollectionFormat = CollectionFormat.Csv
                    ParameterObject.Type = mapSchema param.Schema
                }
            )
            |> Seq.toArray

        apiPaths
        |> Seq.collect (fun pathItem -> 

            let consumes = 
                pathItem.Value.Operations.Values
                |> Seq.collect (fun o ->
                    o.RequestBody.Content.Keys
                )
                |> Seq.toArray

            let produces = 
                pathItem.Value.Operations.Values
                |> Seq.collect (fun o ->
                    o.Responses.Values
                    |> Seq.collect (fun oR ->
                        oR.Content.Keys
                    )
                )
                |> Seq.toArray

            pathItem.Value.Operations
            |> Seq.map (fun operation -> 

                let operationType = mapOperationType operation.Key
                let parameters = mapOperationParameters operation.Value.Parameters

                {
                    OperationObject.Type = operationType
                    OperationObject.Path = pathItem.Key;
                    OperationObject.Description = operation.Value.Description;
                    OperationObject.OperationId  = operation.Value.OperationId;
                    OperationObject.Parameters = parameters;
                    OperationObject.Consumes = consumes;
                    OperationObject.Produces = produces;
                    OperationObject.Responses = [||];
                    OperationObject.Summary = operation.Value.Summary;
                    OperationObject.Tags = operation.Value.Tags |> Seq.map (fun t -> t.Name) |> Seq.toArray;
                    OperationObject.Deprecated = operation.Value.Deprecated;
                }
            )
        )
        |> Seq.toArray

    let parseDefinitions (schemas: IDictionary<string, Models.OpenApiSchema>): (string * SchemaObject) array =
        schemas
        |> Seq.map (fun schema ->
            (schema.Key, mapSchema schema.Value)
        )
        |> Seq.toArray

    let parseTags (tags: IList<Models.OpenApiTag>) =
        tags
        |> Seq.map (fun t -> 
            { TagObject.Name = t.Name; TagObject.Description = t.Description }
        ) 
        |> Seq.toArray;

    let AdaptOpenApiDocument (apiDocument: Models.OpenApiDocument) : SwaggerObject = 

        let serverUrl =  Uri(apiDocument.Servers.[0].Url)
        {
            Info = parseInfo(apiDocument.Info);
            Host = serverUrl.Host;
            BasePath = serverUrl.PathAndQuery;
            Schemes = [| serverUrl.Scheme |];
            Paths = parsePaths(apiDocument.Paths);
            Definitions = parseDefinitions(apiDocument.Components.Schemas);
            Tags = parseTags apiDocument.Tags 
        }

    let Adapt (schemaData: string) : SwaggerObject =
        let apiReader = new OpenApiStringReader()
        let apiReaderDiag = new OpenApiDiagnostic()

        apiReader.Read(schemaData, ref apiReaderDiag)
        |> AdaptOpenApiDocument

module MicrosoftOpenApiParser = 

    let Parse (schemaData: string) =
        let apiReader = new OpenApiStringReader()
        let apiReaderDiag = new OpenApiDiagnostic()

        apiReader.Read(schemaData, ref apiReaderDiag)
