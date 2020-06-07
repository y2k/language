```clojure
(defn view [model]
  (ui/column
    (ui/text model.username)
    (ui/button "ok")))
```

```kotlin
fun Scope.invoke(byteBuffer) =
  when(byteBuffer.readByte()) {
    0x01 -> { // invoke INSTANCE method
      val clsName = readString(byteBuffer)
      val methodName = readString(byteBuffer)
      val argCount = byteBuffer.readByte()
      val args = List(argCount) { invoke(byteBuffer) }
      invokeState(clsName, methodName, args) }
    0x02 -> { // invoke STATIC method
      val methodName = readString(byteBuffer)
      val self = invoke(byteBuffer)
      val argCount = byteBuffer.readByte()
      val args = List(argCount) { invoke(byteBuffer) }
      self.callMethod(methodName, args) }
    0x03 -> { // invoke method
      val methodName = readString(byteBuffer)
      val argCount = byteBuffer.readByte()
      val args = List(argCount) { invoke(byteBuffer) }
      callMethod(methodName, args) }
    0x04 -> readString(byteBuffer) // load STRING const
    0x05 -> readInt(byteBuffer) // load INT const
    0x06 -> { // read field
      val vn = readVariableName(byteBuffer)
      val fn = readFieldName(byteBuffer)
      resolveVariable(vn).readField(fn)
    } }
```

```clojure
(module App

  (defn show [env text]
    (.show 
      (android.widget.Toast/makeText env.context text 0)))

  (defn main [env]
    (show env "Hello")))
```

```clojure
(defn show [env text]
  (.show 
    (android.widget.Toast/makeText env.context text 0)))

(defn main [env]
  (show env "Hello"))
```

```clojure
(defn show_toast [env]
  (let [toast (android.widget.Toast/makeText env.context "Hello" 0)]
    (.show toast)
    (let [toast (android.widget.Toast/makeText env.context "Center - Hello" 0)]
      (.setGravity toast Gravity._CENTER 0 0)
      (.show toast))))
```

```f#
let show_notification text (env : RemoteTransaction.env) =
    let%lwt icon = OcamlUtils.drawable "ic_notification" in
    let%lwt nb = NotificationBuilder.build env.context "default" in
    let%lwt _ = nb#setContentTitle "OCaml remote" in
    let%lwt _ = nb#setContentText text in
    let%lwt _ = nb#setSmallIcon icon in
    let%lwt n = nb#build in
    let%lwt nm = NotificationManager.from env.context in
    nm#notify 1 n

let show_toast (env : RemoteTransaction.env) =
    let%lwt toast = Toast.makeText env.context "Hello" Toast._LENGTH_SHORT in
    toast#show ;%lwt
    let%lwt toast =
        Toast.makeText env.context "from OCaml" Toast._LENGTH_LONG
    in
    toast#setGravity Gravity._CENTER 0 0 ;%lwt
    toast#show
```
