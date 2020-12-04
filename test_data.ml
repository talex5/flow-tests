let message = Cstruct.create 4096
let chunk_size = 10

let () =
  for i = 0 to Cstruct.len message - 1 do
    Cstruct.set_uint8 message i i
  done
