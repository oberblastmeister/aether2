open O

module type S = sig
  module Key : sig
    type t
  end

  type 'a t

  val empty : 'a t
  val singleton : Key.t -> 'a -> 'a t
  val find : 'a t -> Key.t -> 'a option
  val set : 'a t -> key:Key.t -> data:'a -> 'a t
  val remove : 'a t -> Key.t -> 'a t
  val foldi : (Key.t * 'a, 'a t) F.Fold.t
end

module type Finite_map = sig
  module type S = S
end

(* module FromJsMap (M : Map.S) : S with module Key = M.Key and type 'a t = 'a M.t = struct
   module Key = M.Key

   type 'a t = 'a M.t

   let empty = M.empty
   let singleton = M.singleton
   let find = Map.find
   let set = Map.set
   let remove = Map.remove
   let foldi = F.Core.Map.foldi
   end *)
