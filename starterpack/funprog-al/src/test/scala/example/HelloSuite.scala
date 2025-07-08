package example

class HelloSuite extends munit.FunSuite {

  test("The Hello object should say hello") {
    assertEquals(Hello.greeting, "hello")
  }

  test("Hello size is equals to 5") {
    assertEquals(Hello.greeting.length, 5)
  }

  test(
    """ "Hello"(6) should throw a "java.lang.StringIndexOutOfBoundsException" """
  ) {
    intercept[java.lang.StringIndexOutOfBoundsException]("Hello" (6))
  }

}
