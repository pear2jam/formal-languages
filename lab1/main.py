class Coefficient:
    """
    Класс для работы с коэффициэнтами выражения
    Представляет формат репрезентации коэффициентов и арифмитические операции для работы с ними
    """
    def __init__(self, coef):
        """
        Принимает на вход список термов алфавита (ENG|0-9|+)*
        Последовательно идущие элементы означают произведение, знак + означает сумму

        Например: ['a', 'b', '+', 'c'] == ab+c
                  ['-', 'a', '+', 'b'] == -a+b

        coef : list
        return : None
        """
        self.coef = coef


    def __add__(self, y):
        
        """
        Возвращает сумму коэффициентов
        y : Coefficient
        return: Coefficient
        """

        x = self
        if not len(x.coef) : return y
        if not len(y.coef) : return x
        return Coefficient (x.coef + ['+'] + y.coef)

    def __mul__(self, y):
        """
        Возвращает произведение коэффициентов
        y : Coefficient
        return: Coefficient
        """

        x = self
        exp = []
        x_parts, y_parts = [], []
        to_add = []
        for x in x.coef:
            if x == '+' or x == '-':
                if len(to_add):
                    x_parts.append(to_add)
                    to_add = []
            if x != '+': to_add += [x]
        if len(to_add):
            x_parts.append(to_add)
            to_add = []

        for x in y.coef:
            if x == '+' or x == '-':
                if len(to_add):
                    y_parts.append(to_add)
                    to_add = []
            if x != '+': to_add += [x]
        if len(to_add):
            y_parts.append(to_add)
            to_add = []

        for i in x_parts:
            for j in y_parts:
                if i[0] == '-' and j[0] != '-':
                    exp += (i + j)
                elif i[0] != '-' and j[0] == '-':
                    exp += (['-'] + i + j[1:])
                elif i[0] == '-' and j[0] == '-':
                    exp += (['+'] + i[1:] + j[1:])
                else:
                    exp += (['+'] + i + j)
        if exp[0] == '+':
            exp = exp[1:]
        return Coefficient(exp)

class OrdinalCoef:
    """
    Класс для работы с ординальными коэффициентами (выражения вида {sum from i = n to 0 [w^i * a_i]})
    Коэффициенты хранятся как список объектов Coefficient, где i-й элемент равен R коэффициенту при w^(len(list) - i - 1)
    
    Например [2, 1, 3] == (w^2 * 2 + w * 1 + 3)
    """
    def __init__(self, a_list):
        """
        a_list : list - список коэффициентов типа Coefficient a_i при w^i
        """
        self.a_list = a_list
    
    def __add__(self, x):
        """
        Возвращает сумму двух ординальных коэффициентов

        x : OrdinalCoef
        return : OrdinalCoef
        """
        if len(self.a_list) < len(x.a_list):
            return x
        res = self.a_list[::]
        for i in range(-1, -len(x.a_list), -1):
            res[i] = x.a_list[i]
        res[-len(x.a_list)]+= x.a_list[0]
        return OrdinalCoef(res)

    def __mul__(self, x):
        """
        Произведение ординальных коэффициентов
        Перемножение корректно для (wa+b)*(sum w^i*a_i)
        (,то есть len(self) <= 2)

        x : OrdinalCoef
        return : OrdinalCoef
        """
        res = x.a_list[::]
        for i in range(1, len(res)):
            res[i] = x.a_list[i]
        res[-1] = (self.a_list[0]*x.a_list[-1])
        res.append(self.a_list[-1])
        return OrdinalCoef(res)


class Linear:
    """
    Класс для представления линейных функций и работы с ними
    """
    def __init__(self, a, b):
        """
        a, b : OrdinalCoef
        """
        self.a = a
        self.b = b

    def composition(self, g):
        #f = ax+b
        #g = cx+d
        #fg = a(cx+d)+b = acx + ad + b

        """
        Возвращает композицию функций

        g : Linear
        return : Linear
        """

        a_ord = self.a*g.a
        b_ord = self.a*g.b + self.b
        return Linear(a_ord, b_ord)

    def __str__(self):
        """
        Строковое представление функции
        """
        res = ''
        for i in range(len(self.a.a_list)-1):
            res+=('w^'+str(len(self.a.a_list)-i-1)+'*')
            res+='('+ ''.join(self.a.a_list[i].coef) + ')+'
        res+='('+ ''.join(self.a.a_list[-1].coef) + ') X + '
        for i in range(len(self.b.a_list)-1):
            res+=('w^'+str(len(self.b.a_list)-i-1)+'*')
            res+='('+ ''.join(self.b.a_list[i].coef) + ')+'
        res+='('+ ''.join(self.b.a_list[-1].coef) + ')'
        return res


funcs_dict = {}
funcs_list = []
with open('input.txt', 'r') as f:
    for line in f.read().splitlines():
        s_in = line.split('->')    
        left_in, right_in = list(s_in[0]), list(s_in[1])
        funcs = set(left_in+right_in)
        for func in funcs:
            a = OrdinalCoef([Coefficient(['a1'+func]), Coefficient(['a2'+func])])
            b = OrdinalCoef([Coefficient(['b1'+func]), Coefficient(['b2'+func])])
            funcs_dict[func] = Linear(a, b)
        left_f = funcs_dict[left_in[-1]]
        right_f = funcs_dict[right_in[-1]]

        for i in range(-2, -len(left_in)-1, -1):
            left_f = funcs_dict[left_in[i]].composition(left_f)
        for i in range(-2, -len(right_in)-1, -1):
            right_f = funcs_dict[right_in[i]].composition(right_f)
        funcs_list.append((left_f, right_f))




with open('sol.smt2', 'w') as f:
    for i in funcs_dict.keys():
        f.write(f'(declare-const a1{i} Int) \n')
        f.write(f'(declare-const a2{i} Int) \n')
        f.write(f'(declare-const b1{i} Int) \n')
        f.write(f'(declare-const b2{i} Int) \n')

        f.write(f'(assert (>= a1{i} 0)) \n')
        f.write(f'(assert (>= a2{i} 0)) \n')
        f.write(f'(assert (>= b1{i} 0)) \n')
        f.write(f'(assert (>= b2{i} 0)) \n')
    

def smt_coef(c):
    """
    Парсит коэффициент типа Coefficient в smt формат

    c : Coefficient
    return : str
    """
    full_c = ''.join(c.coef).split('+')
    res = ''
    if len(full_c) > 1:
        res += '(+ '
        for i in full_c:
            if len(i) // 3 == 1:
                res += ' ' + i + ' '
                continue
            res += '(* '
            for j in range(len(i)//3):
                res += i[3*j:3*j+3] + ' '
            res += ')'
        res += ')'
        return res
    for j in range(len(full_c[0])//3):
        res = '(*'
        res += ' ' + full_c[0][3*j:3*j+3]
    return res + ')'


def smt_ord_compare(a, b, sign='>'):
    """
    Возвращает сравнение ординалов в формате smt

    a, b : OrdinalCoef
    return : str
    """
    a_list = [Coefficient(['000'])] * max(0, len(b.a_list)-len(a.a_list)) + a.a_list
    b_list = [Coefficient(['000'])] * max(0, len(a.a_list)-len(b.a_list)) + b.a_list

    res = '(or '
    for i in range(len(a_list)):
        res_add = f'({sign} {smt_coef(a_list[i])} {smt_coef(b_list[i])})'
        if i:
            res_add = '(and ' + res_add
        for j in range(i):
            res_add += f'(= {smt_coef(a_list[j])} {smt_coef(b_list[j])})'
        if i:
            res_add += ')'
        res += res_add
    return res + ')'

def smt_ord_equal(a, b):
    """
    Возвращает проверку на равенство ординалов в формате smt

    a, b : OrdinalCoef
    return : str
    """
    a_list = [Coefficient(['000'])] * max(0, len(b.a_list)-len(a.a_list)) + a.a_list
    b_list = [Coefficient(['000'])] * max(0, len(a.a_list)-len(b.a_list)) + b.a_list

    res = '(and '
    for i in range(len(a_list)):
        res += f'(= {smt_coef(a_list[i])} {smt_coef(b_list[i])})'
    return res + ')'

def rule_smt(left_f, right_f):
    """
    Возвращает smt выражение для проверки правила переписывания в формате smt
    left_f, right_f : Linear
    return : str
    """
    return f'(or (and {smt_ord_compare(left_f.a, right_f.a)} {smt_ord_compare(left_f.b, right_f.b, ">=")}) (and {smt_ord_equal(left_f.a, right_f.a)} {smt_ord_compare(left_f.b, right_f.b, ">")}))'


with open('sol.smt2', 'a') as f:
    f.write('(assert (and ')
    for f1, f2 in funcs_list:
        f.write(rule_smt(f1, f2))
    f.write('))')



from z3 import Solver

with open("sol.smt2", "r") as smt2_file:
    smt2_content = smt2_file.read()

solver = Solver()

solver.from_string(smt2_content)


print(str(solver.check()))
