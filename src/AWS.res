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

  @deriving(abstract)
  type query_params<'attributes> = {
    @as("TableName")
    tableName: string,
    @as("Select") @optional
    select: string,
    @as("ScanIndexForward") @optional
    scanIndexForward: bool,
    @as("KeyConditionExpression") @optional
    keyConditionExpression: string,
    @as("ExpressionAttributeValues") @optional
    expressionAttributeValues: 'attributes,
    @as("FilterExpression") @optional
    filterExpression: string,
    @as("ReturnConsumedCapacity") @optional
    returnConsumedCapacity: string,
  }

  @deriving(abstract)
  type query_data<'data> = {
    @as("Count")
    count: int,
    @as("Items")
    items: array<'data>,
    @as("ScannedCount")
    scannedCount: int,
    @as("ConsumedCapacity") @optional
    consumedCapacity: consumedCapacity,
  }

  @send
  external query: (t, query_params<'item>, callback<error, query_data<'data>>) => unit = "query"

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

  @new @module("aws-sdk") @scope("DynamoDB")
  external make: unit => t = "DocumentClient"
}
