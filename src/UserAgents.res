type t

@new @module external make: unit => t = "user-agents"

@send external toString: t => string = "toString"
