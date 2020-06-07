package io.y2k.interpretatorclient

import androidx.test.platform.app.InstrumentationRegistry
import androidx.test.ext.junit.runners.AndroidJUnit4
import kotlinx.coroutines.runBlocking

import org.junit.Test
import org.junit.runner.RunWith

@RunWith(AndroidJUnit4::class)
class ExampleInstrumentedTest {
    @Test
    fun useAppContext() {
        val appContext = InstrumentationRegistry.getInstrumentation().targetContext
//        assertEquals("io.y2k.interpretatorclient", appContext.packageName)

        runBlocking {
            MainActivity.runScript(appContext)
        }

    }
}
