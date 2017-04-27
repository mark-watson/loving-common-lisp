package com.markwatson.deeplearning;

import org.deeplearning4j.datasets.fetchers.CSVDataFetcher;

import java.io.FileInputStream;
import java.io.FileNotFoundException;

/**
 * Created by markw on 10/5/15.
 */
public class LispInterfaceDataFetcher extends CSVDataFetcher {

  public LispInterfaceDataFetcher(String filePath, int labelColumnIndex) throws FileNotFoundException {
    super(new FileInputStream(filePath), labelColumnIndex);
  }
  @Override
  public void fetch(int i) {
    super.fetch(i);
  }

}
