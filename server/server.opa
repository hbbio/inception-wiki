// import stdlib.web.template

db /wiki: stringmap(string)
db /wiki[_] = "This page is empty"

@publish load_source(topic)   = /wiki[topic]
@publish load_rendered(topic) = /wiki[topic]
@publish save_source(topic, source) = do /wiki[topic] <- source
remove_topic(topic) = Db.remove(@/wiki[topic])

rest(topic) =
(
  match HttpRequest.get_method() with
    | {some = method} ->
       match method with
         | {post}   -> 
             do save_source(topic, HttpRequest.get_body()?"") 
             Resource.raw_status({success})
         | {delete} -> do remove_topic(topic) Resource.raw_status({success})
         | {get}    -> Resource.raw_response(load_source(topic), "text/plain", {success})
         | _ -> Resource.raw_status({method_not_allowed})
       end
    | _ -> Resource.raw_status({bad_request})
)

topic_of_path(path) = String.capitalize(String.to_lower(List.to_string_using("", "", "::", path)))

// for Adam: here return the list of urls of existing pages
list_topics() =
  f(acc, x, y) = if acc=="" then x else "{acc},{x}"
  StringMap.fold(f, "", /wiki)

id = parser 
 | id=([0-9a-z]+) -> Text.to_string(id)
uri = parser
 | "_list_?callback=" fname=id ->
   fname = String.flatten(fname)
   "{fname}({list_topics()})"
 | "_rest_/" topic=id "?callback=" fname=id ->
   topic = String.flatten(topic)
   fname = String.flatten(fname)
   "{fname}({rest(topic_of_path(topic))})"

server = Server.simple_server(Resource.raw_response(uri, "text/javascript", {success}))
