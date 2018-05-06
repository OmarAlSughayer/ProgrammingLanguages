# University of Washington, Programming Languages, Homework 7, hw7runner.rb

# Omar Adel AlSughayer, 1337255

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

require 'minitest/autorun'
require_relative './hw7provided'

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  	Extra_Pieces = [rotations([[0, 0], [1, 0], [0, 1], [1, 1], [1, 2]]), # ::.
             rotations([[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]]), # .....
             rotations([[0, 0], [1, 0], [1, 1]])] # .:
  	
  	All_My_Pieces = Extra_Pieces + All_Pieces

  	# The single cheating piece
  	Cheat_Piece = [rotations([[0, 0]])] # .

  # class method to choose the next piece from All_My_Pieces
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end

  # class method to choose the next piece from Cheat_Piece
  def self.cheat_next_piece (board)
  	MyPiece.new(Cheat_Piece.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  
  def initialize (game)
    super
    @current_block = MyPiece.next_piece(self)
    @is_cheating = false
  end

  # gets the information from the current piece about where it is and uses this
  # to store the piece on the board itself.  Then calls remove_filled.
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..locations.size-1).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
      @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end

  # gets the next piece
  def next_piece
  	if !isCheating
  		@current_block = MyPiece.next_piece(self)
    else
    	@current_block = MyPiece.cheat_next_piece(self)
    end

    @current_pos = nil
    @is_cheating = false
  end

  # rotates the current piece 180 degrees
  def rotate_180
  	rotate_clockwise
  	rotate_clockwise
  end

  # changes the current block to the cheating block
  def cheat
  	@score = @score - 100
  	@is_cheating = true
  end

  def isCheating
  	@is_cheating
  end

end

class MyTetris < Tetris
  # your enhancements here

  # creates a canvas and the board that interacts with it
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
  	@root.bind('u', lambda {@board.rotate_180})
  	@root.bind('c', lambda {if @board.score >= 100 and !@board.isCheating 
  							  @board.cheat
  							end})
  	super
  end

end