let sentence = 
  let 
    wordComma = word ^ ","
  in let 
    double = wordComma ^ wordComma
  in let 
    quad = double ^ double
  in let
    oct = quad ^ quad
  in oct ^ word;;