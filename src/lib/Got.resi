type instance
type response<'header> = {
  "statusCode": int,
  "statusMessage": string,
  "headers": 'header,
  "body": string,
}

let create: (string, 'a) => instance
let get: (instance, string, ~params: 'a=?, unit) => Future.t<response<'b>>
let post: (instance, string, 'a) => Future.t<response<'b>>
