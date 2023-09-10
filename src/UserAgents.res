type t

@module("user-agents") external make: unit => t = "random"

@send external toString: t => string = "%identity"
