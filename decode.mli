exception Low_mbar
exception Interpolation_failure
val interpolate :
  values:ZZp.zz array -> points:ZZp.zz array -> d:int -> Poly.t * Poly.t
val factor : Poly.t -> ZZp.Set.t
val reconcile :
  values:ZZp.zz array -> points:ZZp.zz array -> d:int -> ZZp.Set.t * ZZp.Set.t
