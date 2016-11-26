package aftok.bippy

import org.specs2._

class CrossValidate extends Specification {
  def is = s2"""
  Cross validation of bippy BIP-70 implementation against bitcoinj

  Generated BIP-70 payment requests should
    validate using bitcoinj
      
  """
}


