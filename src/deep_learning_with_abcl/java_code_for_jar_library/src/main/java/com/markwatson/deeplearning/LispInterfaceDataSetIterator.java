package com.markwatson.deeplearning;

import org.deeplearning4j.datasets.iterator.BaseDatasetIterator;
import java.io.FileNotFoundException;

public class LispInterfaceDataSetIterator extends BaseDatasetIterator {
  private static final long serialVersionUID = -2023454995728682368L;

  public LispInterfaceDataSetIterator(int batch, int numExamples, String filePath,
                                  int labelColumnIndex) throws FileNotFoundException {
    super(batch, numExamples, new LispInterfaceDataFetcher(filePath, labelColumnIndex));
  }
}