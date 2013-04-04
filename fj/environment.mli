type 'a t

exception Not_bound
exception Error of string
val objcon : Syntax.con
val init_ctable : Syntax.classdec list
val empty : 'a t
val extend : 'a -> 'b -> ('a * 'b) list -> ('a * 'b) list
val ctable : Syntax.classdec list ref
val lookup : 'a -> ('a * 'b) list -> 'b
val map : ('a -> 'b) -> 'a t -> 'b t
val fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b
val append : 'a  list -> 'a list -> 'a list
val reverse : 'a list -> 'a list 
val getSuperClass : Syntax.classdec -> Syntax.classdec
val getClass : Syntax.ty -> Syntax.classdec
val getFields : Syntax.classdec -> (Syntax.ty * Syntax.field) list
val getMethods : Syntax.classdec -> Syntax.mdec list
val tylookup : 'a -> ('b * 'a) list -> 'b
val flookup : Syntax.classdec -> Syntax.field -> Syntax.ty
val mlookup : Syntax.classdec -> string -> Syntax.mdec
val subtype : Syntax.ty -> Syntax.ty -> bool
val isClass : Syntax.ty -> bool
val isvalue : Syntax.term -> bool
