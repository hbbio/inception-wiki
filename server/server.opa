import stdlib.web.template

db /wiki: stringmap(string)
db /wiki[_] = "This page is empty"

@publish load_source(topic)   = /wiki[topic]
@publish load_rendered(topic) = <>{load_source(topic)}</>

@publish save_source(topic, source) =
   /wiki[topic] <- source

remove_topic(topic) = Db.remove(@/wiki[topic])

rest(topic, callback) =
  match HttpRequest.get_method() with
  | {some = method} ->
       match method with
         | {post}   -> _ = save_source(topic, HttpRequest.get_body()?"") Resource.raw_status({success})
         | {delete} -> do remove_topic(topic) Resource.raw_status({success})
         | {get}    -> Resource.raw_response("{callback}(\"{load_source(topic)}\")", "text/javascript", {success})
         | _ -> Resource.raw_status({method_not_allowed})
       end
  | _ -> Resource.raw_status({bad_request})

topic_of_path(path) = String.capitalize(String.to_lower(List.to_string_using("", "", "::", path)))

list_topics() =
  f(acc, path, _content) =
    if acc == "" then path else "{acc},{path}"
  StringMap.fold(f, /wiki, "")

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
