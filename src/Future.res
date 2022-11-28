module Promise = Js.Promise
exception JsError(Js.Exn.t)
exception PromiseError(Promise.error)

type t<'data> = (exn => unit, 'data => unit) => unit

let pure: 'data => t<'data> = (a, _, success) => success(a)

and flat_map: (t<'a>, 'a => t<'b>) => t<'b> = (callback, f, error, success) =>
  callback(error, x => f(x)(error, success))

let apply: (t<'a => 'b>, t<'a>) => t<'b> = (f, a) =>
  flat_map(f, f' => flat_map(a, a' => pure(f'(a'))))

let map: (t<'a>, 'a => 'b) => t<'b> = (a, f) => apply(pure(f), a)

and fork: (t<'data>, exn => unit, 'data => unit) => unit = (future, error, success) =>
  future(err => error(err), data => success(data))

let to_promise: t<'a> => Promise.t<'a> = future =>
  Promise.make((~resolve, ~reject) => fork(future, err => reject(. err), data => resolve(. data)))

and from_promise: Promise.t<'a> => t<'a> = (promise, error, success) =>
  promise
  |> Promise.then_(success' => Promise.resolve(success(success')))
  |> Promise.catch(err => Promise.resolve(error(PromiseError(err))))
  |> ignore

let liftA2: 'a 'b 'c. (('a, 'b) => 'c, t<'a>, t<'b>) => t<'c> = (fn, a, b) =>
  pure(fn)->apply(a)->apply(b)
