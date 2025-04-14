type instance
type cookie
type response<'header> = {
  "statusCode": int,
  "statusMessage": string,
  "headers": 'header,
  "body": string,
}

@module("tough-cookie") @new
external cookie: unit => cookie = "CookieJar"

@module("got") external create': 'a => instance = "extend"

@send
external get': (instance, string) => Js.Promise.t<response<'b>> = "get"

@send
external get0': (instance, string, 'a) => Js.Promise.t<response<'b>> = "get"

@send
external post': (instance, string, 'a) => Js.Promise.t<response<'b>> = "post"

let create = (prefixUrl, headers) =>
  create'({
    "prefixUrl": prefixUrl,
    "headers": headers,
    "cookieJar": cookie(),
  })

let get1' = (i, u, p) => get0'(i, u, {"searchParams": p})

let get = (instance, url, ~params=?, _) =>
  switch params {
  | Some(p) => Future.pure()->Future.flat_map(_ => get1'(instance, url, p)->Future.from_promise)
  | None => Future.pure()->Future.flat_map(_ => get'(instance, url)->Future.from_promise)
  }

let post = (instance, url, form) =>
  Future.pure()->Future.flat_map(_ => post'(instance, url, {"form": form})->Future.from_promise)
