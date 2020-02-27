module Affect = BsEffects.Affect;
let (pure, from_promise) = Affect.(pure, from_promise);

type instance;
type cookie;
type response('header) = {
  .
  "statusCode": int,
  "statusMessage": string,
  "headers": Js.t('header),
  "body": string,
};

[@bs.module "tough-cookie"] [@bs.new]
external cookie: unit => cookie = "CookieJar";

[@bs.module "got"] external create': Js.t('a) => instance = "extend";

[@bs.send]
external get': (instance, string) => Js.Promise.t(response('b)) = "get";

[@bs.send]
external get0': (instance, string, Js.t('a)) => Js.Promise.t(response('b)) =
  "get";

[@bs.send]
external post': (instance, string, Js.t('a)) => Js.Promise.t(response('b)) =
  "post";

let create = (prefixUrl, headers) =>
  create'({
    "prefixUrl": prefixUrl,
    "headers": headers,
    "cookieJar": cookie(),
  });

let get1' = (i, u, p) => get0'(i, u, {"searchParams": p});

let get = (instance, url, ~params=?, _) =>
  Affect.Infix.(
    switch (params) {
    | Some(p) => pure() >>= (_ => get1'(instance, url, p) |> from_promise)
    | None => pure() >>= (_ => get'(instance, url) |> from_promise)
    }
  );

let post = (instance, url, form) =>
  Affect.Infix.(
    pure() >>= (_ => post'(instance, url, {"form": form}) |> from_promise)
  );
