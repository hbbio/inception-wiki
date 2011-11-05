type page = { version : int ; content : intmap(string) ; parent : int }
empty_page = { version = 0 ; content = IntMap.empty ; parent = 0 } : page

db /global: int
db /global = 0

next_version() =
   num = /global + 1
   do /global <- num ;
   num

db /wiki: stringmap(page)
db /wiki[_] = empty_page

@publish load_source(topic) = 
  page = /wiki[topic]
  content = Option.default("Error 42", IntMap.get(page.version, page.content))
  json = { Record = [ ("content", { String = content } : RPC.Json.json),
                      ("version", { Int = page.version } : RPC.Json.json) ] } : RPC.Json.json
  Json.serialize(json)
@publish save_source(topic, source) =
   page = /wiki[topic]
   version = next_version()
   page = { version = version
          ; content = IntMap.add(version, source, page.content)
          ; parent = page.version } : page
   /wiki[topic] <- page

remove_topic(topic) = Db.remove(@/wiki[topic])

rest(topic, callback) =
  match HttpRequest.get_method() with
  | {some = method} ->
       match method with
         | {post}   -> _ = save_source(topic, HttpRequest.get_body()?"") Resource.raw_status({success})
         | {delete} -> do remove_topic(topic) Resource.raw_status({success})
         | {get}    -> Resource.raw_response("{callback}({load_source(topic)})", "text/javascript", {success})
         | _ -> Resource.raw_status({method_not_allowed})
       end
  | _ -> Resource.raw_status({bad_request})

topic_of_path(path) = String.capitalize(String.to_lower(List.to_string_using("", "", "::", path)))

list_topics() =
  f(acc, path, _content) =
    js_path = "\"{path}\""
    if acc == "" then js_path else "{acc}, {js_path}"
  list = StringMap.fold(f, /wiki, "")
  "[{list}]"

get_callback(query) =
  List.assoc("callback", query) ? error("Could not get the callback")

dispatch(uri) =
  match uri with
  | {path=["_list_" | _] ~query fragment=_ is_directory=_ is_from_root=_} ->
      Resource.raw_response("{get_callback(query)}({list_topics()})", "text/javascript", {success})
  | {path=["_rest_" | topic] ~query fragment=_ is_directory=_ is_from_root=_} ->
      rest(topic_of_path(topic), get_callback(query))
  | _ ->
      Resource.raw_status({wrong_address})

server = Server.simple_dispatch(dispatch)
