type instance;
type response('header) = {
  .
  "statusCode": int,
  "statusMessage": string,
  "headers": Js.t('header),
  "body": string,
};

let create: (string, Js.t('a)) => instance;
let get:
  (instance, string, ~params: 'a=?, unit) =>
  BsEffects.Affect.affect(response('b));
let post:
  (instance, string, Js.t('a)) => BsEffects.Affect.affect(response('b));
