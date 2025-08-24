exception DimensionError

module Vector = struct
  type vector = float list

  let create n x =
    if n < 1 then raise DimensionError
    else List.init n (fun _ -> x)

  let dim = List.length

  let is_zero v =
    List.fold_left (fun acc x -> acc && (x = 0.0)) true v

  let unit n j =
    if j < 1 || j > n then raise DimensionError
    else List.init n (fun i -> if i = j - 1 then 1.0 else 0.0)

  let scale c v = List.map (fun x -> c *. x) v

  let addv v1 v2 =
    if dim v1 <> dim v2 then raise DimensionError
    else List.map2 ( +. ) v1 v2

  let dot_prod v1 v2 =
    if dim v1 <> dim v2 then raise DimensionError
    else List.fold_left ( +. ) 0.0 (List.map2 ( *. ) v1 v2)

  let inv v = scale (-1.0) v

  let length v = sqrt (dot_prod v v)

  let angle v1 v2 =
    let dot = dot_prod v1 v2 in
    let magnitudes = length v1 *. length v2 in
    if magnitudes = 0.0 then 0.0  (* Handle zero vectors *)
    else acos (dot /. magnitudes)
end



