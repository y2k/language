package io.y2k.interpretatorclient

import android.content.Context
import android.os.Bundle
import android.view.View
import androidx.appcompat.app.AppCompatActivity
import com.google.gson.GsonBuilder
import com.google.gson.JsonDeserializationContext
import com.google.gson.JsonDeserializer
import com.google.gson.JsonElement
import kotlinx.coroutines.Dispatchers
import kotlinx.coroutines.GlobalScope
import kotlinx.coroutines.launch
import kotlinx.coroutines.withContext
import java.lang.reflect.Type
import java.net.URL

class MainActivity : AppCompatActivity() {

    override fun onCreate(savedInstanceState: Bundle?) {
        super.onCreate(savedInstanceState)
        setContentView(R.layout.activity_main)
    }

    fun test(view: View) {
        GlobalScope.launch(Dispatchers.Main) {
            runScript(this@MainActivity)
        }
    }

    companion object {

        suspend fun runScript(context: Context) {
            val gson = GsonBuilder().registerTypeAdapter(Exp::class.java, Deserializer).create()
            val json = withContext(Dispatchers.IO) { URL("http://192.168.1.72:8080/").readText() }
            val module: Module = gson.fromJson(json, Module::class.java)
            module.callMain(context)
        }
    }
}

object Deserializer : JsonDeserializer<Exp> {
    override fun deserialize(json: JsonElement, typeOfT: Type?, context: JsonDeserializationContext): Exp {
        val barnObject = json.asJsonObject
        return when (val tag = barnObject["@tag"].asString) {
            "str" -> context.deserialize(json, StringValue::class.java)
            "int" -> context.deserialize(json, IntValue::class.java)
            "var" -> context.deserialize(json, Variable::class.java)
            "field" -> context.deserialize(json, ReadField::class.java)
            "call" -> context.deserialize(json, Call::class.java)
            "static-call" -> context.deserialize(json, StaticCall::class.java)
            "instance-call" -> context.deserialize(json, MethodCall::class.java)
            else -> error("Unsupported tag $tag")
        }
    }
}

class Env(@Suppress("unused") @JvmField val context: Context)

class Module(val functions: Map<String, Func>)
data class Func(val argNames: List<String>, val body: List<Exp>)
sealed class Exp
data class StaticCall(val className: String, val methodName: String, val params: List<Exp>) : Exp()
data class MethodCall(val methodName: String, val instance: Exp, val params: List<Exp>) : Exp()
data class Call(val funName: String, val params: List<Exp>) : Exp()
data class StringValue(val value: String) : Exp()
data class IntValue(val value: Int) : Exp()
data class ReadField(val name: String, val field: String) : Exp()
data class Variable(val value: String) : Exp()
