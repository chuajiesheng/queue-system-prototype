open Cryptokit

let hex s = transform_string (Hexa.decode()) s
let tohex s = transform_string (Hexa.encode()) s
