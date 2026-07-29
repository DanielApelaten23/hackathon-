import java.util.*;

public class PythagorasLLM {
    static final int COUNTRY_LEN = 3; // "NGA"

    static int asciiVal(char c) { return (int) c; }

    static int nameFreq(String s) {
        int sum = 0;
        for(char c : s.toCharArray())
            if(Character.isLetter(c)) sum += asciiVal(c);
        return sum % 100;
    }

    static int[] pythagorasMod(int a, int b) {
        int sin = b == 0? 0 : a % b;
        int cos = a == 0? 1 : b % a;
        int tan = cos == 0? 1 : a % cos;
        return new int[]{sin, cos, tan};
    }

    static int[][] generateMatrix(int sin, int cos, int tan, int nameFreq) {
        return new int[][]{
            {sin, cos, nameFreq},
            {tan, COUNTRY_LEN, sin},
            {nameFreq, cos, COUNTRY_LEN}
        };
    }

    public static String predict(String query) {
        String[] words = query.split("\\s+");
        int remainder = 0;
        System.out.println("Input: " + query);

        for(int i=0; i<words.length-1; i+=2){
            int a = asciiVal(words[i].charAt(0));
            int b = asciiVal(words[i+1].charAt(0));
            int[] trig = pythagorasMod(a,b);
            System.out.printf("Chunk: %s %s -> sin=%d cos=%d tan=%d%n",
                words[i], words[i+1], trig[0], trig[1], trig[2]);

            int nf = nameFreq(words[i] + words[i+1]);
            int[][] matrix = generateMatrix(trig[0], trig[1], trig[2], nf);
            remainder = (remainder + trig[0] + trig[1] + trig[2]) % 97;
        }

        int begin = asciiVal(query.charAt(0));
        int end = asciiVal(query.charAt(query.length()-1));
        remainder = (remainder + begin % end) % 97;
        System.out.println("Begin%End: " + begin % end);

        StringBuilder prediction = new StringBuilder();
        for(int cycle=0; cycle<10 && remainder!=0; cycle++){
            int div = cycle + 2;
            remainder = remainder % div;
            prediction.append((char)('a' + remainder % 26));
        }
        System.out.println("Prediction: " + prediction);
        return prediction.toString();
    }

    public static void main(String[] args){
        Scanner sc = new Scanner(System.in);
        System.out.print("Enter query: ");
        String query = sc.nextLine();
        predict(query);
    }
}