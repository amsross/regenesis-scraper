type error = Js.Nullable.t<Js.Exn.t>
type callback<'error, 'return> = ('error, 'return) => unit
type t

/* https://github.com/ahrefs/bs-aws-lambda/blob/1.0.0/src/awsLambda.re#L106 */
module APIGatewayProxy = {
  type t

  module Event = {
    @deriving(abstract)
    type t = {
      body: Js.Nullable.t<string>,
      headers: Js.Dict.t<string>,
      httpMethod: string,
      isBase64Encoded: bool,
      path: string,
      pathParameters: Js.Nullable.t<Js.Dict.t<string>>,
      queryStringParameters: Js.Nullable.t<Js.Dict.t<string>>,
      stageVariables: Js.Nullable.t<Js.Dict.t<string>>,
      /* requestContext: EventRequestContext.t, */
      resource: string,
    }
    let make = t
  }

  type context

  module Result = {
    @deriving(abstract)
    type t = {
      statusCode: int,
      headers: Js.Dict.t<Js.Json.t>,
      body: string,
    }
    let make = t
  }

  type handler<'event> = ('event, context) => Js.Promise.t<Result.t>
}

module DynamoDB = {
  type t
  type db = t

  @deriving(abstract)
  type consumedCapacity = {
    @as("TableName")
    tableName: string,
    @as("CapacityUnits")
    capacityUnits: float,
    @as("ReadCapacityUnits")
    readCapacityUnits: float,
    @as("WriteCapacityUnits")
    writeCapacityUnits: float,
  }

  module Decode = {
    type t = {
      n: option<string>,
      s: option<string>,
    }

    let obj = Json.Decode.object(field => {
      n: field.optional(. "N", Json.Decode.string),
      s: field.optional(. "S", Json.Decode.string),
    })

    let int = obj->Json.Decode.map((. {n}) => {
      try {
        n->Belt.Option.map(int_of_string)->Belt.Option.getExn
      } catch {
      | _ => raise(Json.Decode.DecodeError("could not convert to int"))
      }
    })

    let float = obj->Json.Decode.map((. {n}) => {
      try {
        n->Belt.Option.map(float_of_string)->Belt.Option.getExn
      } catch {
      | _ => raise(Json.Decode.DecodeError("could not convert to float"))
      }
    })

    let string = obj->Json.Decode.map((. {s}) => {
      try {
        s->Belt.Option.getExn
      } catch {
      | _ => raise(Json.Decode.DecodeError("could not convert to string"))
      }
    })
  }

  /* https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-dynamodb/classes/putitemcommand.html */
  module PutItemCommand = {
    type t

    /* https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-dynamodb/interfaces/putitemcommandinput.html */
    module Input = {
      @deriving(abstract)
      type t = {
        @as("ConditionExpression") @optional conditionExpression: string,
        @as("ExpressionAttributeNames") @optional
        expressionAttributeNames: Js.Dict.t<Js.Dict.t<string>>,
        @as("ExpressionAttributeValues") @optional
        expressionAttributeValues: Js.Dict.t<Js.Dict.t<string>>,
        @as("Item") item: Js.Dict.t<Js.Dict.t<string>>,
        @as("ReturnConsumedCapacity") @optional
        returnConsumedCapacity: @string [#INDEXES | #TOTAL | #NONE],
        @as("ReturnItemCollectionMetrics") @optional
        returnItemCollectionMetrics: @string [#SIZE | #NONE],
        @as("ReturnValues") @optional returnValues: @string [#NONE | #ALL_OLD],
        @as("TableName") tableName: string,
      }
    }

    /* https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-dynamodb/interfaces/putitemcommandoutput.html */
    module Output = {
      @deriving(abstract)
      type rec t = {
        @as("Attributes") attributes: Js.Dict.t<Js.Dict.t<string>>,
        @as("CounsumedCapacity") consumedCapacity: consumedCapacity,
        @as("ItemCollectionMetrics") itemCollectionMetrics: Js.Nullable.t<itemCollectionMetrics>,
      }
      and itemCollectionMetrics = {
        @as("ItemCollectionKey") itemCollectionKey: string,
        @as("SizeEstimateRangfeGB") sizeEstimateRangfeGB: (int, int),
      }
    }

    @new @module("@aws-sdk/client-dynamodb")
    external make: Input.t => t = "PutItemCommand"

    @send
    external send: (db, t) => Js.Promise.t<Output.t> = "send"
  }

  /* https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-dynamodb/classes/querycommand.html */
  module QueryCommand = {
    type t

    /* https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-dynamodb/interfaces/querycommandinput.html */
    module Input = {
      @deriving(abstract)
      type t = {
        @as("ExpressionAttributeNames") @optional
        expressionAttributeNames: Js.Dict.t<Js.Dict.t<string>>,
        @as("ExpressionAttributeValues") @optional
        expressionAttributeValues: Js.Dict.t<Js.Dict.t<string>>,
        @as("FilterExpression") @optional filterExpression: string,
        @as("KeyConditionExpression") @optional keyConditionExpression: string,
        @as("ReturnConsumedCapacity") @optional
        returnConsumedCapacity: @string [#INDEXES | #TOTAL | #NONE],
        @as("ScanIndexForward") @optional scanIndexForward: bool,
        @as("Select") @optional
        select: @string
        [#ALL_ATTRIBUTES | #ALL_PROJECTED_ATTRIBUTES | #COUNT | #SPECIFIC_ATTRIBUTES],
        @as("TableName") tableName: string,
      }
    }

    /* https://docs.aws.amazon.com/AWSJavaScriptSDK/v3/latest/clients/client-dynamodb/interfaces/querycommandoutput.html */
    module Output = {
      type t = {
        @as("CounsumedCapacity") consumedCapacity: consumedCapacity,
        @as("Count") count: Js.Nullable.t<int>,
        @as("Items") items: Js.Nullable.t<array<Js.Json.t>>,
        @as("LastEvaluatedKey") lastEvaluatedKey: Js.Nullable.t<Js.Dict.t<Js.Json.t>>,
        @as("ScannedCount") scannedCount: Js.Nullable.t<int>,
      }
    }

    @new @module("@aws-sdk/client-dynamodb")
    external make: Input.t => t = "QueryCommand"

    @send
    external send: (db, t) => Js.Promise.t<Output.t> = "send"
  }

  @deriving(abstract)
  type put_params<'item> = {
    @as("TableName")
    tableName: string,
    @as("Item")
    item: 'item,
    @as("ReturnConsumedCapacity") @optional
    returnConsumedCapacity: string,
  }

  @deriving(abstract)
  type put_data = {
    @as("ConsumedCapacity") @optional
    consumedCapacity: consumedCapacity,
  }

  @send
  external put: (t, put_params<'a>, callback<error, put_data>) => unit = "put"

  @new @module("@aws-sdk/client-dynamodb")
  external make: unit => t = "DynamoDBClient"
}
