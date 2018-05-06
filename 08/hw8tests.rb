# University of Washington, Programming Languages, Homework 8
# Omar Adel AlSughayer, 1337255


require 'minitest/autorun'
require_relative './hw8assignment'

# unit tests for Polynomial
class TestPoly < Minitest::Test

	def test_all
		@x = "x".asPolynomial
    @y = "y".asPolynomial

    # basic to_s
		assert_equal("Polynomial(x)", @x.to_s)
    assert_equal("Polynomial(y)", @y.to_s)
    
    assert_equal("Polynomial(2*x*y + 3)", (2*@x*@y + 3).to_s)
    
    # tests the full functionality of * over +
    assert_equal("Polynomial(x*y + 3*y)", ((@x+3)*@y).to_s)

    # tests -
    assert_equal("Polynomial(x + -8)", (@x-8).to_s)
	  
    # tests that zero terms are removed from the polu
    assert_equal("Polynomial(x*x + -1)", ((@x+1)*(@x-1)).to_s)

    # tests that a single zero poly is printed if there are no other terms
    assert_equal("Polynomial(0)", ((3*@x+5)*0).to_s)
    
    # tests that terms are arranged alphabetacally
    assert_equal("Polynomial(3*x + 10*y)", (10*@y+3*@x).to_s)
    
    # tests commulitivity 
    assert_equal("Polynomial(5*x*y)", (2*@x*@y + @x*3*@y).to_s)

    # tests commulitivity and arranging the elements within a term
    assert_equal("Polynomial(16*x*y + 11)", (10*@x*@y + 1 + @y*3*@x*2 + 10).to_s)
    
    # tests that variables could be anything 
    assert_equal("Polynomial(squid)", ("squid".asPolynomial).to_s)
  end

end
