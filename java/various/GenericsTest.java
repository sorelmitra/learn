import java.util.*;

class OldCollection {
    public void test() {
        List l = new ArrayList();
        l.add(new String("str1"));
        l.add(new String("str2"));
        l.add(new Integer(3)); // generates runtime exception at print
        printCollection(l);
    }

    private void printCollection(Collection c) {
        Iterator i = c.iterator();
        while(i.hasNext()) {
            String item = (String) i.next();
            System.out.println("Item: "+item);
        }
    }
}

class GenericsCollection {
    public void test() {
        List<String> l = new ArrayList<String>();
        l.add(new String("str1"));
        l.add(new String("str2"));
        // l.add(new Integer(3)); // won't compile
        printCollection(l);
    }

    private void printCollection(Collection c) {
        Iterator<String> i = c.iterator();
        while(i.hasNext()) {
            System.out.println("Item: "+i.next());
        }
    }
}

class GenericsTest {
    public static void main(String argv[]) {
        GenericsCollection gen = new GenericsCollection();
        gen.test();
        OldCollection old = new OldCollection();
        old.test();
    }
}
