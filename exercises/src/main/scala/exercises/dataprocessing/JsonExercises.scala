package exercises.dataprocessing

object JsonExercises {

  sealed trait Json
  case class JsonNumber(number: Double)         extends Json
  case class JsonString(text: String)           extends Json
  case class JsonObject(obj: Map[String, Json]) extends Json
  case class JsonBoolean(boolean: Boolean)      extends Json
  case class JsonArray(array: Array[Json])      extends Json
  case class JsonNull()                         extends Json

  def trimAll(json: Json): Json =
    json match {
      case _: JsonNumber | _: JsonNull => json
      case JsonString(str)             => JsonString(str.trim)
      case JsonObject(obj) =>
        val newObj = obj.map { case (key, value) =>
          (key, trimAll(value))
        }
        JsonObject(newObj)
      case JsonArray(array) => JsonArray(array.map(trimAll))
    }

  // a. Implement `anonymize`, a method which keeps the structure of the JSON document
  // but removes all data such as:
  // * all `JsonString` are replaced by `***`.
  // * all `JsonNumbers` are replaced by 0
  // For example:
  // {                                          {
  //  "name": "John Doe",                         "name": "***",
  //  "age": 25,                                  "age": 0,
  //  "address": {                                "address": {
  //    "street": {             anonymize           "street": {
  //      "number" : 12,           ==>                "number" : 0,
  //      "name" : "Cody road"                        "name" : "***"
  //    },                                          },
  //    "country": "UK",                            "country": "***",
  //  }                                           }
  //}                                           }
  def anonymize(json: Json): Json =
    json match {
      case JsonNumber(_) => JsonNumber(0)
      case JsonString(_) => JsonString("***")
      case JsonObject(obj) =>
        val newObj = obj.map { case (key, value) =>
          (key, anonymize(value))
        }
        JsonObject(newObj)
      case _: JsonNull      => json
      case JsonArray(array) => JsonArray(array.map(anonymize))
    }

  // b. Implement `search`, a method that checks if a JSON document contains a text.
  // Note: `search` doesn't look inside of the keys of a `JsonObject`, only the values.
  // For example:
  // * search({ }, "ll") == false
  // * search(5, "ll") == false
  // * search("Hello", "ll") == true
  // * search({ "message" : "hello" }, "ll") == true
  // * search({ "message" : "hi" }, "ll") == false
  def search(json: Json, text: String): Boolean =
    json match {
      case JsonNumber(_) | JsonNull() => false
      case JsonString(value)          => value.contains(text)
      case JsonObject(obj) =>
        obj.exists { case (_, value) =>
          search(value, text)
        }
      case JsonArray(array) => array.exists(search(_, text))
    }

  def searchWithDepthLimit(json: Json, text: String, depthLimit: Int): Boolean = {
    if (depthLimit < 0) return false
    json match {
      case JsonNumber(_) | JsonNull() => false
      case JsonString(value)          => value.contains(text)
      case JsonObject(obj) =>
        obj.exists { case (_, value) =>
          searchWithDepthLimit(value, text, depthLimit - 1)
        }
      case JsonArray(array) => array.exists(searchWithDepthLimit(_, text, depthLimit - 1))
    }
  }

  // c. Implement `depth`, a method that calculates the maximum level of nesting of a JSON document.
  // For example:
  // * { }, 5 or "hello" have depth 0
  // * { "name" : "john" } has depth 1
  // * { "name" : "john", "address" : { "postcode" : "E16 4SR" } } has depth 2
  def depth(json: Json): Int =
    json match {
      case JsonNumber(_) | JsonString(_) | JsonNull() => 0
      case JsonObject(obj) =>
        obj.values.map(depth).maxOption.fold(0)(_ + 1)
      case JsonArray(array) => array.map(depth).maxOption.getOrElse(0)
    }

  //////////////////////////////////////////////
  // Bonus question (not covered by the video)
  //////////////////////////////////////////////

  // d. Add the missing cases to JSON enumeration: JsonBoolean, JsonArray and JsonNull.

  // e. add an extra parameter to search so that it limits the depth of the search.
  // such as search({ "user" : { "name" : "John" } }, "o", 2) == true
  // but     search({ "user" : { "name" : "John" } }, "o", 1) == false because "John" is at depth 2

}
