package io.y2k.interpretatorclient

import android.content.Context
import java.lang.reflect.Method
import java.lang.reflect.Modifier

fun Module.callMain(ctx: Context) {
    invoke("main", mapOf("env" to Env(ctx)))
}

fun Module.invoke(funName: String, scope: Map<String, Any>): Any? {
    fun Exp.invokeExp(scope: Map<String, Any?>): Any? = when (this) {
        is Call -> {
            val args = params.map { it.invokeExp(scope) }
            val f = functions.getValue(this.funName)
            val innerScope = f.argNames.mapIndexed { i, an -> an to args[i] }.toMap()
            f.body.map { it.invokeExp(innerScope) }.last()
        }
        is StaticCall -> {
            val args = params.map { it.invokeExp(scope) }
            val cls = Class.forName(className)
            val method = findStaticMethod(cls, methodName, args)
            method.invoke(null, *args.toTypedArray())
        }
        is MethodCall -> {
            val args = params.map { it.invokeExp(scope) }
            val self = instance.invokeExp(scope)!!
            val method = findInstanceMethod(self, methodName)
            method.invoke(self, *args.toTypedArray())
        }
        is ReadField -> {
            val v = scope.getValue(name)!!
            val field = v.javaClass.declaredFields.find { it.name == field }!!
            field.get(v)
        }
        is Variable -> scope.getValue(value)
        is StringValue -> value
        is IntValue -> value
    }
    return functions.getValue(funName).body.map { it.invokeExp(scope) }.last()
}

private fun findStaticMethod(cls: Class<*>, methodName: String, args: List<Any?>): Method {
    return cls.declaredMethods
        .filter { Modifier.isStatic(it.modifiers) }
        .filter { it.name == methodName }
        .filter { it.parameterTypes.size == args.size }
        .filter { method ->
//            method.parameterTypes.all {
//                it.isAssignableFrom()
//            }
            method.parameterTypes.zip(args).all { (t, p) ->
                val a = fixTypeToGeneric(t).isAssignableFrom(p!!.javaClass)
//                val a = p!!.javaClass.isAssignableFrom(t)
                a
            }
        }
        .single()
}

private fun fixTypeToGeneric(t: Class<*>): Class<*> =
    when (t) {
        Int::class.java -> java.lang.Integer::class.java
        else -> t
    }

private fun findInstanceMethod(self: Any, methodName: String): Method {
    return self.javaClass.methods
        .filter { !Modifier.isStatic(it.modifiers) }
        .find { it.name == methodName } ?: error("Can't find method '$methodName' in $self")
}
