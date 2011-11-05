type page = { version : int ; content : intmap(string) ; parent : int }
empty_page = { version = 0 ; content = IntMap.empty ; parent = 0 } : page

db /global: int
db /global = 0

get_global() = /global
next_version() =
   num = /global + 1
   do /global <- num ;
   num

db /wiki: stringmap(page)
db /wiki[_] = empty_page

@publish load_source(topic, version) = 
  page = /wiki[topic]
  do jlog("load = topic:{topic} version:{page.version}")
  content = Option.default("This page is empty", IntMap.get(page.version, page.content))
  json = { Record = [ ("content", { String = content } : RPC.Json.json),
                      ("version", { Int = page.version } : RPC.Json.json),
                      ("parent", { Int = page.parent } : RPC.Json.json) ] } : RPC.Json.json
  Json.serialize(json)
@publish save_source(topic, source) =
   page = /wiki[topic]
   version = next_version()
   page = { version = version
          ; content = IntMap.add(version, source, page.content)
          ; parent = page.version } : page
   do jlog("save = topic:{topic} version:{version}")
   do /wiki[topic] <- page
   void
@public get_version() =
   json = { Int = get_global() } : RPC.Json.json
   Json.serialize(json)

remove_topic(topic) = Db.remove(@/wiki[topic])

rest(topic, callback, version) =
  do jlog("rest: version={version}")
  match HttpRequest.get_method() with
  | {some = method} ->
       match method with
         | {post}   -> _ = save_source(topic, HttpRequest.get_body()?"") Resource.raw_status({success})
         | {delete} -> do remove_topic(topic) Resource.raw_status({success})
         | {get}    -> Resource.raw_response("{callback}({load_source(topic, version)})", "text/javascript", {success})
         | _ -> Resource.raw_status({method_not_allowed})
       end
  | _ -> Resource.raw_status({bad_request})

topic_of_path(path) = String.capitalize(String.to_lower(List.to_string_using("", "", "::", path)))

list_topics() =
  f(path, _content, acc) =
    // do jlog("acc:{acc}")
    js_path = "\"{path}\""
    if acc == "" then js_path else "{acc}, {js_path}"
  list = StringMap.fold(f, /wiki, "")
  "[{list}]"

get_callback(query) =
  List.assoc("callback", query) ? error("Could not get the callback")

dispatch(uri) =
  match uri with
  | {path=["_create_" | _] query=_ fragment=_ is_directory=_ is_from_root=_} ->
     do save_source("Test", "test")
     do save_source("Inception", "inception")
     do save_source("Test", "second test")
     Resource.raw_response("done", "plain/text", {success})
  | {path=["_list_" | _] ~query fragment=_ is_directory=_ is_from_root=_} ->
      Resource.raw_response("{get_callback(query)}({list_topics()})", "text/javascript", {success})
  | {path=["_version_" | _] ~query fragment=_ is_directory=_ is_from_root=_} ->
      Resource.raw_response("{get_callback(query)}({get_version()})", "text/javascript", {success})
  | {path=["_rest_" | topic ] ~query fragment=_ is_directory=_ is_from_root=_} ->
      // todo: separate topic, version
      rest(topic_of_path(topic), get_callback(query), 0)
  | _ ->
      Resource.raw_status({wrong_address})

server = Server.simple_dispatch(dispatch)
