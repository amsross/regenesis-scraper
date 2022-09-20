type instance
type response<'header> = {
  "statusCode": int,
  "statusMessage": string,
  "headers": 'header,
  "body": string,
}

let create: (string, 'a) => instance
let get: (instance, string, ~params: 'a=?, unit) => BsEffects.Affect.affect<response<'b>>
let post: (instance, string, 'a) => BsEffects.Affect.affect<response<'b>>
