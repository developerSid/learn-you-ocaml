open Result

module Text : sig
   type text
   type text2 = private Text2 of text
   type trimmedText2 = private TrimmedText2 of text2

   val create_text: string -> text
   val create_text2: string -> (string -> 'error) -> (text2, 'error) result
end = struct
   type text = string
   type text2 = Text2 of text
   type trimmedText2 =  TrimmedText2 of text2 

   let create_text s = s
   let create_text2 s too_long_error_fn =
      if String.length s <= 2 then
         Ok (Text2 s)
      else
         Error (too_long_error_fn s)
end

