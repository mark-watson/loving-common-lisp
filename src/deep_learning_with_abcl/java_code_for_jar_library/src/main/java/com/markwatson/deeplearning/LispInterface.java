package com.markwatson.deeplearning;

import java.io.FileNotFoundException;

/**
 * Created by markw on 4/24/17.
 */
public class LispInterface {
  private static DeepBeliefNetworkLispInterfaceData trainedNetwork = null;
  public static Object trainNetwork(int numInput, int numHidden, int numberOfLayers,
                             int labelColumnIndex, int numSamples,
                             int iterations, String trainingCsvFilePath)  throws Exception {
    trainedNetwork = new DeepBeliefNetworkLispInterfaceData();
    DeepBeliefNetworkLispInterfaceData.train(numInput, numHidden, numberOfLayers,
                                         labelColumnIndex, numSamples,
                                         iterations, trainingCsvFilePath);
    return "done training";
  }

  public static int [] evaluateData (String dataFilePath) throws FileNotFoundException {
    return DeepBeliefNetworkLispInterfaceData.evaluateNewData(dataFilePath);
  }

  // test in Java:
  public static void main(String [] args) throws Exception {
    System.out.println(trainNetwork(9, 3, 3,
                                    9, 48,
                                    100, "data/training.csv"));
    int [] evaluated_classes = evaluateData("data/try_it_out.csv");
    for (int i=0; i<evaluated_classes.length; i++) {
      System.out.println("row in test CSV file: " + (i + 1) + ", evaluated class for row: " + evaluated_classes[i]);
    }
  }
}
