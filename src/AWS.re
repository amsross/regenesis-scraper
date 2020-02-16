type error = Js.Nullable.t(Js.Exn.t);
type callback('error, 'return) = ('error, 'return) => unit;
type t;

/* https://github.com/ahrefs/bs-aws-lambda/blob/1.0.0/src/awsLambda.re#L106 */
module APIGatewayProxy = {
  type t;

  module Event = {
    [@bs.deriving abstract]
    type t = {
      body: Js.Nullable.t(string),
      headers: Js.Dict.t(string),
      httpMethod: string,
      isBase64Encoded: bool,
      path: string,
      pathParameters: Js.Nullable.t(Js.Dict.t(string)),
      queryStringParameters: Js.Nullable.t(Js.Dict.t(string)),
      stageVariables: Js.Nullable.t(Js.Dict.t(string)),
      /* requestContext: EventRequestContext.t, */
      resource: string,
    };
    let make = t;
  };

  type context;

  module Result = {
    [@bs.deriving abstract]
    type t = {
      statusCode: int,
      headers: Js.Dict.t(Js.Json.t),
      body: string,
    };
    let make = t;
  };

  type handler = (Event.t, context) => Js.Promise.t(Result.t);
};

module DynamoDB = {
  type t;

  [@bs.deriving abstract]
  type consumedCapacity = {
    [@bs.as "TableName"]
    tableName: string,
    [@bs.as "CapacityUnits"]
    capacityUnits: float,
    [@bs.as "ReadCapacityUnits"]
    readCapacityUnits: float,
    [@bs.as "WriteCapacityUnits"]
    writeCapacityUnits: float,
  };

  [@bs.deriving abstract]
  type query_params = {
    [@bs.as "TableName"]
    tableName: string,
    [@bs.as "Select"] [@bs.optional]
    select: string,
    [@bs.as "ScanIndexForward"] [@bs.optional]
    scanIndexForward: bool,
    [@bs.as "KeyConditionExpression"] [@bs.optional]
    keyConditionExpression: string,
    [@bs.as "ExpressionAttributeValues"] [@bs.optional]
    expressionAttributeValues: Js.Dict.t(string),
    [@bs.as "FilterExpression"] [@bs.optional]
    filterExpression: string,
    [@bs.as "ReturnConsumedCapacity"] [@bs.optional]
    returnConsumedCapacity: string,
  };

  [@bs.deriving abstract]
  type query_data = {
    [@bs.as "Count"]
    count: int,
    [@bs.as "Items"]
    items: array(Js.Dict.t(string)),
    [@bs.as "ScannedCount"]
    scannedCount: int,
    [@bs.as "ConsumedCapacity"] [@bs.optional]
    consumedCapacity,
  };

  [@bs.send]
  external query: (t, query_params, callback(error, query_data)) => unit =
    "query";

  [@bs.deriving abstract]
  type put_params = {
    [@bs.as "TableName"]
    tableName: string,
    [@bs.as "Item"]
    item: Js.Dict.t(string),
    [@bs.as "ReturnConsumedCapacity"] [@bs.optional]
    returnConsumedCapacity: string,
  };

  [@bs.deriving abstract]
  type put_data = {
    [@bs.as "ConsumedCapacity"] [@bs.optional]
    consumedCapacity,
  };

  [@bs.send]
  external put: (t, put_params, callback(error, put_data)) => unit = "put";

  [@bs.new] [@bs.module "aws-sdk"] [@bs.scope "DynamoDB"]
  external make: unit => t = "DocumentClient";
};
