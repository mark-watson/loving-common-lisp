import org.armedbear.lisp.*;

public class Simple {
  
  static public String modifyString(String s) {
      return s + "123";
  }
    static public int addTwoNumbers(int a, int b) {
	      return a + b;
    }
    static public Object modifyArray(Object values) {
      System.out.println("* modifyArray: values = " + values);
	      return values;
    }
}