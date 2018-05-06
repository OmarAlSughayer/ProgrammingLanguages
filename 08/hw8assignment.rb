# Omar Adel AlSughayer, 1337255
# CSE 341, Assignment 08

require 'minitest/autorun'

class Polynomial
  
  # initializes a new Polynomial object with a PolyTerm
  def initialize (pTerms)
    @terms = Array.new()
    if pTerms != nil
      @terms = (@terms + pTerms)
    end
  end

  # adds another poly to self
  def + (poly)
    poly = assurePoly(poly)

    @newTerms = Array.new(self.getTerms)

    (0..poly.getTerms.size-1).each { |pi|
        @index = @newTerms.index(poly.getTerms[pi])

        if @index != nil
            @newTerms[@index] = @newTerms[@index] + poly.getTerms[pi]
        else
          @newTerms = @newTerms << poly.getTerms[pi]
        end
      }

    # delete all terms with zero coeff
    @newTerms.delete_if { |term| term.getCoeff == 0}
    
    if @newTerms.size == 0
      @newTerms << PolyTerm.new(nil, 0)
    end

    @newTerms = @newTerms.sort

    return Polynomial.new(@newTerms)
  end

  # subtracts another poly from self
  def - (poly)
    poly = assurePoly(poly)

    @negativeTerms = Array.new()

    (0..poly.getTerms.size-1).each{|i| 
      @negativeTerms << poly.getTerms[i]*PolyTerm.new(nil, -1)
      }

    return self.+(Polynomial.new(@negativeTerms))
  end

  # multiplies another poly by self
  def * (poly)

    poly = assurePoly(poly)

    @resultPoly = Polynomial.new(nil)

    (0..self.getTerms.size-1).each { |si|  
      (0..poly.getTerms.size-1).each { |pi| 
        @resultTerm = self.getTerms[si] * poly.getTerms[pi]
        @resultPoly = @resultPoly + Polynomial.new([@resultTerm])
       }}

    return @resultPoly
  end

  # return a srting representation of this poly
  def to_s
    @form = "Polynomial("

    if self.getTerms != nil
      @form += self.getTerms[0].to_s
    end

    (1..self.getTerms.size-1).each { |i|  @form += " + " + self.getTerms[i].to_s}

    @form += ")"

    return @form
  end

  # gets called when we try to +/-/* a non-Poly with a Poly
  def coerce (np)
    return [np.asPolynomial, self]
  end

  protected 
  # returns the terms of this poly
  def getTerms
    return @terms
  end
  protected

  private 
  # transforms poly to a Polynomial if it was not already
  def assurePoly(poly)
    begin
      poly = poly.asPolynomial
    rescue NoMethodError
      # do nothing
    end

    return poly
  end
  private 

end

class PolyTerm
  
  # initializes a new PolyTerm object with a list of variables and a coeff
  def initialize (var, coeff)
     @vars = nil 
     if var != nil
        @vars = var.sort
     end 
     @coeff = coeff
  end

  # adds another term to this term
  def + (term)
    if term.getVar == self.getVar and self.getVar != nil
      return PolyTerm.new(Array.new(self.getVar), self.getCoeff + term.getCoeff)
    elsif term.getVar == self.getVar and self.getVar == nil
      return PolyTerm.new(nil, self.getCoeff + term.getCoeff)
    else
      return false
    end
  end

  # subtracts another term from this poly
  def - (term)
    if term.getVar == self.getVar and self.getVar != nil
      return PolyTerm.new(Array.new(self.getVar), self.getCoeff - term.getCoeff)
    elsif term.getVar == self.getVar and self.getVar == nil
      return PolyTerm.new(nil, self.getCoeff - term.getCoeff)
    else
      return false
    end
  end

  # multiplys another term to this poly
  def * (term)
    @newVar = nil
    @newCoeff = term.getCoeff*self.getCoeff

    # create the new var
    if self.getVar == nil and term.getVar != nil
      @newVar = Array.new(term.getVar)
    elsif self.getVar != nil and term.getVar == nil
      @newVar = Array.new(self.getVar)
    elsif self.getVar == nil and term.getVar == nil
      @newVar = nil
    else
      @newVar = Array.new(term.getVar + self.getVar)
    end

    #sort the new var
    if @newVar != nil
      @newVar = @newVar.sort
    end

    return PolyTerm.new(@newVar, @newCoeff)
  end

  # defines the equality between PolyTerm's as having the same variables
  def == (term)
    return term.getVar == self.getVar
  end

  # defines terms comparision by the alphabatical order of their elements
  def <=> (term)
    if term.getVar == self.getVar
      return 0
    else

      if self.getVar == nil
        return 1
      elsif term.getVar == nil
        return -1
      end

      # the shortest size
      @minSize = [term.getVar.size, self.getVar.size].min

      (0..@minSize-1).each { |i| 
        @result = self.getVar[i] <=> term.getVar[i]

        if @result != 0
          return @result
        end 
      }

      # the longest term comes first if they have equal heads
      return self.getVar.size <=> term.getVar.size
    end
  end

  # returns a string represintation of this poly 
  def to_s
    @form = ""
    
    if (self.getCoeff != nil and self.getVar == nil) or self.getCoeff != 1
      @form += self.getCoeff.to_s
    end

    if self.getCoeff != nil and self.getCoeff != 1 and self.getVar != nil
      @form += "*"
    end
      
    if self.getVar != nil
      (0..self.getVar.size-2).each { |i|  @form += self.getVar[i] + "*"}
      @form += self.getVar[self.getVar.size-1]
    end

    return @form
  end

  # returns the variables
  def getVar
    return @vars
  end

  # returns the coeff
  def getCoeff
    return @coeff
  end
end

# basic Numeric class, to add functionality to it
class Numeric
  def asPolynomial
    Polynomial.new([PolyTerm.new(nil, self)])
  end
end

# basic String class, to add functionality to it
class String
  def asPolynomial
    Polynomial.new([PolyTerm.new([self], 1)])
  end
end