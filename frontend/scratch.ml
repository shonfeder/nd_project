(** Drag and drop example *)

(* let position ?(attrs=[]) ?(content=[]) () =
 *   (\* TODO refactor out *\)
 *   let style = Attr.style @@ Css_gen.(
 *       border ~style:`Solid () @>
 *       height (`Px 60) @>
 *       margin_top (`Px 20)
 *     )
 *   in
 *   let ondrop = Attr.on "drop" (fun _ev ->
 *       Jsoo.Firebug.console##log "ondrop triggered";
 *       inject Action.Incr_counter (\* TODO replace with drop function *\))
 *   in
 *   let ondragover = Attr.on "dragover" (fun ev ->
 *       Jsoo.Firebug.console##log "ondragover triggered";
 *       Jsoo.Dom.preventDefault ev;
 *       Event.Ignore
 *     )
 *   in
 *   let class_ = Attr.class_ "position"  in
 *   let attrs = ondragover :: ondrop :: style :: class_ :: attrs in
 *   Node.div attrs content
 * in
 * let occupant = [Node.p [Attr.create "draggable" "true"] [Node.text "Drag Me"]]
 * in
 * let%map from_box =
 *   (\* TODO Focus change on specific dom element  *\)
 *   let%map _ = m in
 *   position ~content:occupant ()
 * and to_box =
 *   let%map _ = m in
 *   position () *)
