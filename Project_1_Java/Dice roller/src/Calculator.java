import java.util.*;

class Dice { // object for processing dice
    int dAmount; //number of dice
    int dSize;   //size of the dice
}

public class Calculator {

    public int listProcessing(String data) { //this is the function that I will use to process the user input into dice numbers

//      converting the string input into a list of stings to be processed
        List<String> dataList = Arrays.asList(data.split(""));
        Dice die = new Dice();
        int inum = 0;
        int isub = 0;
        int isub2 = 0;
        boolean add = false;
        boolean advantage = false;
        boolean disadvantage = false;
        int total = 0;
        String lastnonnumber = "";
        int Dloc = 0;



        //checking for advantage
        int size = dataList.size();
        String advCheck = "";
        int sizeCheckadv = 3;
        for (int i = 0; i < 3; i++) {
            advCheck += (dataList.get(dataList.size()-sizeCheckadv));
            sizeCheckadv --;
        }

        if (Objects.equals(advCheck, "adv")) {
            advantage = true;
            for (int i = 0; i < 3; i++) {dataList.removeLast();}
        } else if (Objects.equals(advCheck, "dis")) {
            disadvantage = true;
            for (int i = 0; i < 3; i++) {dataList.removeLast();}
        }

        int spacei =0;
        for (String dataListItem : dataList){    //removing empty spaces
            if (dataListItem == " ") {
                dataList.remove(spacei);
            }
            spacei++;
        }

        for (int iall = 0; iall <= dataList.size(); iall++) { //start of iteration for calculation
            String dataListItem = null;
            if (iall < dataList.size()) {// an attempt to get the last number located
                 dataListItem = dataList.get(iall);
            }else { dataListItem = "~~~";}
            dataListItem = dataListItem.toUpperCase();

            if (Objects.equals(dataListItem, "+")) { //getting modifier
                add = true;
            }

            if (Objects.equals(dataListItem, "D")) {//getting the first value for how many dice}
                lastnonnumber = "D";
                isub = inum - 1;
                Dloc = inum;
    
                List<String> dinum = new ArrayList<>(); //temp list
    
                for (int i = isub; i < inum; i++) {
                    dinum.add(dataList.get(i));
                }
                String tempstring = null;
                if (dinum.size() > 1) {
                    tempstring = String.join("", dinum);//combining any multiple numbers to one string
                } else {
                    tempstring = dinum.getFirst();
                }
    
                die.dAmount = parseNumber(tempstring);
    
            }

            else if ((Objects.equals(dataListItem, "+") || Objects.equals(dataListItem, "-") || Objects.equals(dataListItem, "~~~")) && Objects.equals(lastnonnumber, "+")) { //getting additional modifiers to total

                total += parseNumber(dataList.get(inum-1) );
            }
    

            else if ((Objects.equals(dataListItem, "+") || Objects.equals(dataListItem, "-")|| Objects.equals(dataListItem, "~~~")) && Objects.equals(lastnonnumber, "D")) {// getting second modifier
                lastnonnumber = "+";
                isub2 = inum - 1;

    
                List<String> disize = new ArrayList<>(); //temp list
    
                for (int i = Dloc + 1; i < isub2+1; i++) { //starts the check at the second number
                    disize.add(dataList.get(i));
                }
                String tempstring = null;
                if (disize.size() > 1) {
                    tempstring = String.join("", disize);//combining any multiple numbers to one string
                } else {
                    tempstring = disize.getFirst();
                }
                die.dSize = parseNumber(tempstring);
    
                total += rollTheDice(die, advantage, disadvantage);
            }
        


                inum++;

        }
        return total;
    }

    public static int parseNumber(String foNumber) { // this is the way I decided to parse the numbers into integers if they were able to

        try { // trying to see if a number parses, if it does it returns the number otherwise it returns a failcode
            return Integer.parseInt(foNumber);
        } catch (NumberFormatException e) {
            return 54; //failcode
        }
    }
    
   public static int rollTheDice(Dice die, boolean adv, boolean dis) { // rolls the dice
       int total = 0;
       Random roll = new Random();
       if (adv) {// rolling with advantage
           
           int roll1 = 0;
           int roll2 = 0;

           
           
           for (int i = 0; i < die.dAmount; i++) {
               int rolling = roll.nextInt(die.dSize)+1;
               roll1 += rolling;
           }

           for (int i = 0; i < die.dAmount; i++) {
               int rolling = roll.nextInt(die.dSize)+1;
               roll2 += rolling;
           }

           total = Math.max(roll1, roll2);

       } else if (dis) {//rolling with disadvantage
           
           int roll1 = 0;
           int roll2 = 0;



           for (int i = 0; i < die.dAmount; i++) {
               int rolling = roll.nextInt(die.dSize)+1;
               roll1 += rolling;
           }

           for (int i = 0; i < die.dAmount; i++) {
               int rolling = roll.nextInt(die.dSize)+1;
               roll2 += rolling;
           }

           total = Math.min(roll1, roll2);
       } else {
           int rollSingle = 0;
           for (int i = 0; i < die.dAmount; i++) {
               int rolling = roll.nextInt(die.dSize)+1;
               rollSingle += rolling;
           }
           return rollSingle;
       }

        return total;
   }



}
