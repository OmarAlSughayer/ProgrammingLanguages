# University of Washington, Programming Languages, Homework 7, hw7runner.rb

# Omar Adel AlSughayer, 1337255

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

require 'minitest/autorun'
require_relative './hw7provided'
require_relative './hw7assignment'

# unit tests for all the enhancements
class TestTetris < Minitest::Test

	# tests the first enhancement, using the u button to rotate a piece 180 degrees
	def test_u
		@board = MyTestBoard.new
		@piece = MyPiece.next_piece(@board)

		@board.rotate_180
		@u_rotation = @piece.current_rotation
		@board.rotate_counter_clockwise
		@board.rotate_counter_clockwise
		@board.rotate_clockwise
		@board.rotate_clockwise
		@flip_rotation = @piece.current_rotation

		assert_equal(@u_rotation, @flip_rotation)
	end

  # tests that all 10 pieces are being produced
  def test_pieces
    @board = MyTestBoard.new
    
    all_test_pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
               MyPiece.rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
               [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
               [[0, 0], [0, -1], [0, 1], [0, 2]]],
               MyPiece.rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
               MyPiece.rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
               MyPiece.rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
               MyPiece.rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
               MyPiece.rotations([[0, 0], [1, 0], [0, 1], [1, 1], [1, 2]]), # ::.
               MyPiece.rotations([[0, 0], [-1, 0], [1, 0], [2, 0], [3, 0]]), # .....
               MyPiece.rotations([[0, 0], [1, 0], [1, 1]])] # .:

    all_pieces_rotations = []    

    # get all the individual rotations into an array
    (0..all_test_pieces.size-1).each { |oi|  
          all_pieces_rotations += all_test_pieces[oi]
    }
    
    # get a new piece with a specific rotation and delete it
    (0..1000).each { |index|  
        @piece = MyPiece.next_piece(@board)
        all_pieces_rotations.delete(@piece.current_rotation)}

    # all pieces have been generated and deleted
    assert_equal([], all_pieces_rotations)

    # same for the cheating piece
    cheat_piece = MyPiece.rotations([[0, 0]]) # .
    @piece = MyPiece.cheat_next_piece(@board)
    cheat_piece.delete(@piece.current_rotation)
    assert_equal([], cheat_piece)
  end

  # test the third enhancement, the cheating piece 
  def test_cheat
    # create a board and save the score
    @board = MyTestBoard.new
    @old_score = @board.score

    # cheat then get the new score and the next piece
    @board.cheat
    @new_score = @board.score
    @board.next_piece
    @new_piece = @board.current_block

    # the score should be less by a 100 and the next piece should be the cheating piece
    assert_equal(@new_score, @old_score - 100)
    assert_equal(@new_piece.current_rotation, [[0, 0]])
  end

end

# a subclass of MyBoard that does not need a Tetris object to work
class MyTestBoard < MyBoard

  def initialize
    @grid = Array.new(num_rows) {Array.new(num_columns)}
    @score = 0
    @delay = 500
    @current_block = MyPiece.next_piece(self)
    @is_cheating = false
  end

  def rotate_clockwise
      @current_block.move(0, 0, 1)
  end

  def rotate_counter_clockwise
      @current_block.move(0, 0, -1)
  end

  def current_block
    @current_block
  end
end

