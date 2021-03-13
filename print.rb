class Print
  attr_reader :register

  def initialize(register)
    @register = register
  end

  def r_format(instruction)
    rs = register[instruction.rs.to_i(2)]
    rt = register[instruction.rt.to_i(2)]
    rd = register[instruction.rd.to_i(2)]
    shamt = instruction.shamt
    funct = instruction.funct

    puts funct_to_print(funct: funct, rs: rs, rt: rt, rd: rd, shamt: shamt)
  end

  def i_format(instruction)
    rs = register[instruction.rs.to_i(2)]
    rt = register[instruction.rt.to_i(2)]
    addr_or_val = instruction.addr_or_val
    op = instruction.op

    puts opcode_to_print(op: op, rs: rs, rt: rt, addr_or_val: addr_or_val)
  end

  def j_format(instruction)
    addr_or_val = instruction.target_adress
    op = instruction.op

    puts opcode_to_print(op: op, addr_or_val: addr_or_val)
  end

  private

  def opcode_to_print(op:, rs: '', rt: '', addr_or_val: '')
    case op
    when '000010'
      "j #{addr_or_val.to_i(2)}"
    when '000011'
      "jal #{addr_or_val.to_i(2)}"
    when '000100'
      "beq #{rs}, #{rt}, #{addr_or_val.to_i(2)}"
    when '000101'
      "bne #{rs}, #{rt}, #{addr_or_val.to_i(2)}"
    when '001000'
      "addi #{rt}, #{rs}, #{addr_or_val.to_i(2)}"
    when '001100'
      "andi #{rt}, #{rs}, #{addr_or_val.to_i(2)}"
    when '001001'
      "addiu #{rt}, #{rs}, #{addr_or_val.to_i(2)}"
    when '001101'
      "ori #{rt}, #{rs}, #{addr_or_val.to_i(2)}"
    when '001111'
      "lui #{rt}, #{addr_or_val.to_i(2)}"
    when '100011'
      "lw #{rt}, #{addr_or_val.to_i(2)}(#{rs})"
    when '101011'
      "sw #{rt}, #{addr_or_val.to_i(2)}(#{rs})"
    else
      ''
    end
  end

  def funct_to_print(funct:, rs: '', rt: '', rd: '', shamt: '')
    case funct
    when '000000'
      "sll #{rd}, #{rt}, #{shamt.to_i(2)}"
    when '000010'
      "srl #{rd}, #{rt}, #{shamt.to_i(2)}"
    when '001000'
      "jr #{rs}"
    when '001100'
      'syscall'
    when '100000'
      "add #{rd}, #{rs}, #{rt}"
    when '100001'
      "addu #{rd}, #{rs}, #{rt}"
    when '101010'
      "slt #{rd}, #{rs}, #{rt}"
    else
      ''
    end
  end
end
