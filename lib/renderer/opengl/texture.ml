open Core.Syntax.Result

type t =
  { width : int;
    heigth : int;
    id : int
  }
  [@@warning "-69"]

let create path =
  match Bimage_io.read Bimage.Type.u8 Bimage.Color.rgb path with
  | Ok image ->
    let width, heigth, _ = Bimage.Image.shape image in
    let id = Gl.create_texture Gl.Texture_2D in
    let* _ = Gl.texture_storage_2d id 1 Gl.Rgb8 width heigth in
    let* _ = Gl.texture_parameteri id Gl.Texture_min_filter Gl.Linear in
    let* _ = Gl.texture_parameteri id Gl.Texture_mag_filter Gl.Nearest in
    let data = Bimage.Image.data image in
    let+ _ = Gl.texture_sub_image_2d id 0 0 0 width heigth Gl.Rgb Gl.UnsignedByte data in
    { width;
      heigth;
      id;
    }
  | Error err ->
    ( match err with
    | `File_not_found t -> Fmt.failwith "File not found: %s" t
    | (`Invalid_shape | `Invalid_kernel_shape _ | `Invalid_input _ | `Invalid_color | `Msg _) as err
      ->
      failwith @@ Bimage.Error.to_string err )

let bind slot s = Ok (Gl.bind_texture_unit slot s.id)

let finalize s = Ok (Gl.delete_texture s.id)

