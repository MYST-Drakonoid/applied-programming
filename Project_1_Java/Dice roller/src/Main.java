import java.util.Random;
import java.util.Scanner;



public class Main {
    public static void main(String[] args) {
        String input = "";
        Scanner sc = new Scanner(System.in);

        System.out.println("Please enter a roll: ");
        System.out.println("eg: 1d20 +5");
        input = sc.nextLine();
        Calculator calc = new Calculator();


        String noSpaces = input.replace(" ", "");//removes any spaces


            int output = calc.listProcessing(noSpaces);
            System.out.println(output);

            System.out.println("Error please try again");


    }
}