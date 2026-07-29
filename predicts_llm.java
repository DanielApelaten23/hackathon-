import java.util.*;

public class PythagorasLLM {
    static final String COUNTRY = "NGA";

    public static int asciiVal(char c) { return (int)c; }

    public static int nameFrequency(String s) {
        int sum = 0;
        for(char c : s.toCharArray())
            if(Character.isLetter(c)) sum += asciiVal(c);
        return sum % 100;
    }

    public static int[] pythagorasMod(int a, int b) {
        int sin = a % b;
        int cos = (a == 0)? 1 : b % a;
        int tan = (cos == 0)? 1 : a % cos;
        return new int[]{sin, cos, tan};
    }

    public static int[][] generateMatrix(int sin, int cos, int tan, int freq) {
        return new int[][]{
            {sin, cos, freq},
            {tan, COUNTRY.length(), sin},
            {freq, cos, COUNTRY.length()}
        };
    }

    public static void predict(String query) {
        String[] words = query.split("\\s+");
        int remainder = 0;
        System.out.println("Input: " + query);

        for(int i=0; i<words.length-1; i+=2){
            int a = asciiVal(words[i].charAt(0));
            int b = asciiVal(words[i+1].charAt(0));
            int[] trig = pythagorasMod(a,b);
            int freq = nameFrequency(words[i]+words[i+1]);
            int[][] matrix = generateMatrix(trig[0], trig[1], trig[2], freq);

            System.out.printf("Chunk: %s %s -> sin=%d cos=%d tan=%d%n",
                words[i], words[i+1], trig[0], trig[1], trig[2]);
            remainder = (remainder + trig[0] + trig[1] + trig[2]) % 97;
        }

        int begin = asciiVal(query.charAt(0));
        int end = asciiVal(query.charAt(query.length()-1));
        remainder = (remainder + begin % end) % 97;
        System.out.println("Begin%End: " + (begin % end));

        StringBuilder pred = new StringBuilder();
        for(int cycle=0; cycle<10 && remainder!=0; cycle++){
            int div = cycle + 2;
            remainder = remainder % div;
            char c = (char)('a' + remainder % 26);
            pred.append(c);
            System.out.printf("Cycle %d: rem=%d char=%c%n", cycle+1, remainder, c);
        }
        System.out.println("Final Prediction: " + pred);
    }

    public static void main(String[] args){
        predict("I going to the market");
    }
}