module Apply = (
  T: {
    type t<'a>
    let map: (t<'a>, 'a => 'b) => t<'b>
    let flatMap: (t<'a>, 'a => t<'b>) => t<'b>
  },
) => {
  let ap: 'a 'b. (T.t<'a => 'b>, T.t<'a>) => T.t<'b> = (f, m) => T.flatMap(f, f => T.map(m, f))

  let liftA2: 'a 'b 'c. (T.t<'a>, T.t<'b>, ('a, 'b) => 'c) => T.t<'c> = (a, b, f) =>
    ap(T.map(a, f), b)

  let liftA3: 'a 'b 'c 'd. (T.t<'a>, T.t<'b>, T.t<'c>, ('a, 'b, 'c) => 'd) => T.t<'d> = (
    a,
    b,
    c,
    f,
  ) => a->T.map(f)->ap(b)->ap(c)

  let liftA4: 'a 'b 'c 'd 'e. (
    T.t<'a>,
    T.t<'b>,
    T.t<'c>,
    T.t<'d>,
    ('a, 'b, 'c, 'd) => 'e,
  ) => T.t<'e> = (a, b, c, d, f) => a->T.map(f)->ap(b)->ap(c)->ap(d)
}
