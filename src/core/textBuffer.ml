
let lines file =
  let contents = In_channel.with_open_bin file In_channel.input_all in
  String.split_on_char '\n' contents

let list_to_array list = 
    let list_size = List.length list in
    let new_array = Array.make list_size "" in
    let rec list_to_array i = function
      hd :: tl -> new_array.(i) <- hd; list_to_array (i+1) tl
    | _        -> new_array in
    list_to_array 0 list