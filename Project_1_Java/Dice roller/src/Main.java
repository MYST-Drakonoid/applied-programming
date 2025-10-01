import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
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
            try {
                System.out.println(output);
                outputFile(output);
                System.out.println("result printed to output.txt");
            } catch (Exception e) {
                System.out.println("Error please try again");
            }



    }

    public static void outputFile(int data) {
        File file = new File("output.txt");

        if (file.exists()) {
            try (FileWriter writer = new FileWriter("output.txt", true)) { // overwrite mode
                writer.write(String.valueOf(data));
            } catch (IOException e) {
                e.printStackTrace();}
        } else {
            try (FileWriter writer = new FileWriter("output.txt")) { // overwrite mode
                writer.write(String.valueOf(data));
            } catch (IOException e) {
                e.printStackTrace();}
        }
    }
}