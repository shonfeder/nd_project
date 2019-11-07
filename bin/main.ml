open Opium.Std

let js =
  Middleware.static
    (* TODO move to a nicer place? *)
    ~local_path:"./_build/default/frontend"
    ~uri_prefix:"/js"
    ()

let index =
  Middleware.static
    ~local_path:"./public"
    ~uri_prefix:"/"
    ()

let root =
  get "/" begin fun _req ->
    redirect' (Uri.of_string "/index.html")
  end

let () =
  print_endline (Sys.getcwd ());
  App.empty
  |> middleware js
  |> middleware index
  |> root
  |> App.run_command
