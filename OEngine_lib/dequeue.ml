type 'a elem = {
  mutable previous : 'a elem option;
  mutable next : 'a elem option;
  value : 'a;
}

type 'a t = DQnil | DQueue of { front : 'a elem; back : 'a elem }

let nil = DQnil

let make value =
  let elem = { previous = None; next = None; value } in
  DQueue { front = elem; back = elem }

let rec length_aux len = function
  | { next = None; _ } -> len + 1
  | { next = Some elem; _ } -> length_aux (len + 1) elem

let length = function DQnil -> 0 | DQueue { front; _ } -> length_aux 0 front

let append a = function
  | DQnil ->
      let elem = { previous = None; next = None; value = a } in
      DQueue { front = elem; back = elem }
  | DQueue { front; back } ->
      let elem = { previous = Some back; next = None; value = a } in
      back.next <- Some elem;
      DQueue { front; back = elem }

let prepend a = function
  | DQnil ->
      let elem = { previous = None; next = None; value = a } in
      DQueue { front = elem; back = elem }
  | DQueue { front; back } ->
      let elem = { previous = None; next = Some front; value = a } in
      front.previous <- Some elem;
      DQueue { front = elem; back }

let pop = function
  | DQnil -> DQnil
  | DQueue ({ front; back } as dq) ->
      (match front with
      | { next = None; _ } -> DQnil
      | _ when front = back -> DQnil
      | { next = Some e; _ } ->
          (match e.next with Some n -> n.previous <- Some e | None -> ());
          DQueue { dq with front = e })

let pop_back = function
  | DQnil -> DQnil
  | DQueue ({ front; back } as dq) ->
      (match back with
      | { previous = None; _ } -> DQnil
      | _ when back = front -> DQnil
      | { previous = Some e; _ } ->
          (match e.previous with Some p -> p.previous <- Some e | None -> ());
          DQueue { dq with back = e })

let find_elem a = function
  | DQnil -> None
  | DQueue { front; _ } ->
      let rec find_aux = function
        | { value; _ } as e when value = a -> Some e
        | { next = Some n; _ } -> find_aux n
        | _ -> None
      in
      find_aux front

let remove a = function
  | DQnil -> DQnil
  | DQueue (_ as elems) as dq ->
      (match find_elem a dq with
      | Some e ->
          (match (e.previous, e.next) with
          | Some p, Some n ->
              p.next <- Some n;
              n.previous <- Some p;
              DQueue elems
          | Some p, None ->
              p.next <- None;
              DQueue elems
          | None, Some n ->
              n.previous <- None;
              DQueue elems
          | None, None -> DQnil)
      | None -> dq)

let nth dq n =
  if n < 0 then
    invalid_arg "Dequeue.nth"
  else
    let rec nth_aux e n =
      match e with
      | { value; _ } when n = 0 -> value
      | { next = None; _ } -> failwith "nth"
      | { next = Some e; _ } -> nth_aux e (n - 1)
    in
    match dq with
    | DQnil -> failwith "nth"
    | DQueue { front; _ } -> nth_aux front n

let nth_opt dq n =
  if n < 0 then
    invalid_arg "Dequeue.nth"
  else
    let rec nth_aux e n =
      match e with
      | { value; _ } when n = 0 -> Some value
      | { next = None; _ } -> None
      | { next = Some e; _ } -> nth_aux e (n - 1)
    in
    match dq with DQnil -> None | DQueue { front; _ } -> nth_aux front n

let append_dequeue dq1 dq2 =
  match (dq1, dq2) with
  | DQnil, _ -> dq2
  | _, DQnil -> dq1
  | DQueue { front; back = eb }, DQueue { front = ef; back } ->
      eb.next <- Some ef;
      ef.previous <- Some eb;
      DQueue { front; back }

let iter f = function
  | DQnil -> ()
  | DQueue { front; _ } ->
      let rec iter_aux = function
        | { next = None; value; _ } -> f value
        | { next = Some next; value; _ } ->
            f value;
            iter_aux next
      in
      iter_aux front

let iter_while_false f = function
  | DQnil -> ()
  | DQueue { front; _ } ->
      let rec iter_aux = function
        | { next = None; value; _ } -> ignore @@ f value
        | { next = Some next; value; _ } ->
            if f value then
              ()
            else
              iter_aux next
      in
      iter_aux front

let iter_back f = function
  | DQnil -> ()
  | DQueue { back; _ } ->
      let rec iter_aux = function
        | { previous = None; value; _ } -> f value
        | { previous = Some previous; value; _ } ->
            f value;
            iter_aux previous
      in
      iter_aux back

let fold_left f acc = function
  | DQnil -> acc
  | DQueue { front; _ } ->
      let rec fold_left_aux f acc = function
        | { next = None; value; _ } -> f acc value
        | { next = Some e; value; _ } -> fold_left_aux f (f acc value) e
      in
      fold_left_aux f acc front

let fold_left_back f acc = function
  | DQnil -> acc
  | DQueue { back; _ } ->
      let rec fold_left_aux f acc = function
        | { previous = None; value; _ } -> f acc value
        | { previous = Some e; value; _ } -> fold_left_aux f (f acc value) e
      in
      fold_left_aux f acc back

let (fold_right : ('a -> 'b -> 'b) -> 'a t -> 'b -> 'b) =
 fun f dq acc ->
  match dq with
  | DQnil -> acc
  | DQueue { front; _ } ->
      let rec fold_right_aux f e acc =
        match e with
        | { next = None; value; _ } -> f value acc
        | { next = Some e; value; _ } -> fold_right_aux f e (f value acc)
      in
      fold_right_aux f front acc

let fold_right_back f dq acc =
  match dq with
  | DQnil -> acc
  | DQueue { back; _ } ->
      let rec fold_right_aux f e acc =
        match e with
        | { previous = None; value; _ } -> f value acc
        | { previous = Some e; value; _ } -> fold_right_aux f e (f value acc)
      in
      fold_right_aux f back acc
