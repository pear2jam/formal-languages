from automata import Automata

import random
random.seed(10)


def check_balance(s):
    # соблюдается ли баланс скобок
    balance = 0
    for i in s:
        if i == '(': balance += 1
        elif i == ')': balance -= 1
        if balance < 0: return False
    if balance: return False
    return True


def parse_block(r):
    # возращает тип блока регулярного выражения и нормальную форму
    # 'LA - lookahead', 'LB' - lookbehind, 'CM' - common
    while r[0] == '(' and r[-1] == ')' and check_balance(r[1:-1]):
        r = r[1:-1]
    
    if len(r) > 3 and r[0:2] == '?=' and r[-1] == '$':
        return 'LAD', r[2:-1]
    elif len(r) > 2 and r[0:2] == '?=':
        return 'LA', r[2:]
    if len(r) > 3 and r[0:2] == '<=':
        return 'LB', r[2:]
    else:
        return 'CM', r


class ReTree:
    """
    Дерево регулярного выражения
    value - значение узла
    children : list - список потомков (None если потомков нет)
    """
    def __init__(self, value, children):
        self.value = value
        self.children = children
    def __getitem__(self, idx):
        return self.children[idx]
    def __str__(self):
        return f'{self.value} (with {len(self.children)} children)'



def parse_regex(r):
    # Избавляемся от ограничивающих знаков
    if len(r) and r[0] == '^': r = r[1:]
    if len(r) and r[-1] == '$': r = r[:-1]
    # Останавливаемся на символах и пустых строках
    if len(r) == 0:
        return ReTree('empty', [])
        
    if len(r) == 1:
        return ReTree(r, [])
    
    # Избавляемся от оборачивающих скобок
    # '((a+b))' -> 'a+b'
    
    while r[0] == '(' and r[-1] == ')' and check_balance(r[1:-1]) and parse_block(r)[0] == 'CM':
        r = r[1:-1]
    
    
    # Парсим выражение на множетели конкатенации
    #'a|bc(a+b)' -> ['a', '|', 'b', 'c', '(a+b)']

    exps = []
    balance = 0
    to_add = ''
    for s in r:
        if s == '*' and not balance:
            exps[-1] += '*'
            continue

        to_add += s
        if s == '(': balance += 1
        if s == ')': balance -= 1
        
        if balance == 0:
            exps.append(to_add)
            to_add = ''
    

    
    # Добавляем узел итерации и передаем выражение дальше (как детей узла итерации)

    if len(exps) == 1 and exps[0][-1] == '*':
        return ReTree('cycle', [parse_regex(exps[0][:-1])])
    
    # Парсим альтернативу
    # 'a|b(b+c)|cd' -> ['a', 'b(b+c)', 'cd']
    
    

    or_regex = [i for i,x in enumerate(exps) if x=='|']
    if or_regex:
        children = []
        children.append(''.join(exps[:or_regex[0]]))
        children += [''.join(exps[or_regex[i]+1 : or_regex[i+1]]) for i in range(len(or_regex)-1)]
        children.append(''.join(exps[or_regex[-1]+1:]))
        return ReTree('or', list(map(parse_regex, children)))

    
    # Проверяем на lookahead и lookbehind
    # Если есть одновременно оба типа проверок: разбиваем на два выражения с одним типов внутри
    # По формулам a(?=b$)c == a(b*c) и a(<=b$)c == (a*b)c избавляемся от проверочных выражений
    la_list, lb_list = list(), list()
    la_flag, lb_flag = False, False
    la_ind, lb_ind = 1e9, -1e9
    exp_types = []
        
    for i, exp in enumerate(exps):
        exp_type, exp = parse_block(exp)
        exp_types.append(exp_type)
        #exps[i] = exp
        if exp_type == 'CM':
            la_list.append(exp)
            lb_list.append(exp)
        if exp_type == 'LA' or exp_type == 'LAD':
            la_list.append(exp)
            la_flag = True
            la_ind = min(la_ind, i)
        if exp_type == 'LB': 
            lb_list.append(exp)
            lb_flag = True
            lb_ind = max(lb_ind, i)
            
    
    #print(exps)
    if la_flag and lb_flag:
        list_a, list_b = [], []
        return ReTree('inter', [parse_regex(''.join(la_list)), parse_regex(''.join(lb_list))])
    
    if la_flag:
        a, b, c = ''.join(exps[:la_ind]), parse_block(exps[la_ind])[1], ''
        
        if la_ind + 1 < len(exps): 
            c = ''.join(exps[la_ind+1:])
        if exp_types[la_ind] == 'LA':
            b += '(a|b|c|d|e|f|g|h|m|n|p|r|s|t)*'
        return ReTree('and', [parse_regex(a), ReTree('inter', [parse_regex(b), parse_regex(c)])])
        #return ReTree('and', [parse_regex(a), ReTree('inter', [parse_regex(b + c), parse_regex(c)])])
        #else:
        #   return ReTree('and', [parse_regex(a), ReTree('inter', [parse_regex(b), parse_regex(c)])])
    
    if lb_flag:
        a, b, c = ''.join(exps[:lb_ind]), parse_block(exps[lb_ind])[1], ''
        if lb_ind + 1 < len(exps): c = ''.join(exps[lb_ind+1:])
        return ReTree('and', [ReTree('inter', [parse_regex(a), parse_regex(b)]), parse_regex(c)])

    # Парсим конкатенацию (в случае если нет альтернативы и проверок)
    # 'a(b+c)d'-> ['a', '(b+c)', 'd']
    
    return ReTree('and', list(map(parse_regex, exps)))




def regex_to_automata(reg_tree):
    # рекурсивно собирает автомат по дереву регулярного выражения
    if reg_tree.value == 'and':
        aut = regex_to_automata(reg_tree.children[0])
        for i in range(1, len(reg_tree.children)):
            aut = aut.concat(regex_to_automata(reg_tree.children[i]))
        return aut
    
    if reg_tree.value == 'or':
        aut = regex_to_automata(reg_tree.children[0])
        for i in range(1, len(reg_tree.children)):
            aut = aut.parallel(regex_to_automata(reg_tree.children[i]))
        return aut
    
    if reg_tree.value == 'inter':
        aut = regex_to_automata(reg_tree.children[0])
        for i in range(1, len(reg_tree.children)):
            aut = aut.product(regex_to_automata(reg_tree.children[i]))
        return aut
    
    if reg_tree.value == 'cycle':
        aut = regex_to_automata(reg_tree.children[0])
        return aut.iteration()
    
    if reg_tree.value == 'empty':
        return Automata({0}, set(), {}, 0, 0)
    
    else:
        # для букв
        return Automata({0, 1}, {reg_tree.value}, {0: [[reg_tree.value, 1]]}, 0, 1)



def mul_term(a, b):
    if a == 'ε': a = ''
    if b == 'ε': b = ''
    return a+b

def mul(a, b):
    res = []
    for i in a:
        for j in b: res.append(mul_term(i, j))
    return res

def resample_term(a, c):
    return [a*i for i in range(int(c/len(a))+2)]


def resample(a, max_l):
    b = parse_regex(a)
    if b.value == 'cycle':
        b = [b]
    elif b.value != 'and': return set([a])
    else:
        b = b.children
    res = ['ε']
    for i in b:
        if i.value == 'cycle':
            res = mul(res, resample_term(i[0].value, max_l-len(b)))
        else:
            res = mul(res, i.value)
    return set(res)


def equal(a, b):
    a = resample(a, max(len(a), len(b)))
    b = resample(b, max(len(a), len(b)))
    #print(a, b)
    if len(a.intersection(b)): return True
    return False


def simplify(a):
    a = sorted(a)
    seen = []
    for i in range(len(a)):
        for j in range(i+1, len(a)):
            if equal(a[i], a[j]): 
                
                if a[i].count('*') > a[j].count('*') : seen.append(a[j])
                elif a[i].count('*') == a[j].count('*'):
                    if len(a[i]) < len(a[j]): seen.append(a[j])
                    else: seen.append(a[i])
                else: seen.append(a[i])
                    
    return list(set(a).difference(set(seen)))
                

def norm_reg(reg_tree):
    if reg_tree.value == 'and':
        res = ['ε']
        for i in reg_tree.children:
            res = mul(res, norm_reg(i))
        return list(set(res))
    if reg_tree.value == 'or':
        res = []
        for i in reg_tree.children:
            res += norm_reg(i)
        return list(set(res))
    
    if reg_tree.value == 'empty':
        return ''
    
    if reg_tree.value == 'cycle':
        return ['(' + '|'.join(norm_reg(reg_tree.children[0])) + ')*']
    
    else:
        return reg_tree.value


def _norm_reg(reg_tree):
    if reg_tree.value == 'and':
        return ''.join([_norm_reg(i) for i in reg_tree.children if i.value != 'ε'])
    
    elif reg_tree.value == 'or':
        vals = [i.value for i in reg_tree.children]
        eps = True
        if 'cycle' in vals: eps = False
        to_parallel = []
        for i in reg_tree.children:
            if i.value == 'ε':
                if eps:
                    to_parallel.append(i)
                    eps = False
            else:
                to_parallel.append(i)
        
        if len(to_parallel) > 1: l, r = '(', ')'
        else: l, r = '', ''
        
        return l + '|'.join(list(set(map(_norm_reg, to_parallel)))) + r
    
    elif reg_tree.value == 'cycle':
        body = _norm_reg(reg_tree.children[0])
        if len(body) > 1 and not check_balance(body[1:-1]) and body[0] == '(' and body[1] == ')': return '(' + body + ')*'
        if len(body) > 1 and (body[0] != '(' or body[-1] != ')'): return '(' + body + ')*'
        else: return body + '*'
    
    elif reg_tree.value == 'empty':
        return ''
    
    
    return reg_tree.value

def norm_regex(r):
    reg_tree = parse_regex(r)
    reg = _norm_reg(reg_tree)
    
    while reg[0] == '(' and reg[-1] == ')' and check_balance(reg[1:-1]):
        reg = reg[1:-1]
    
    reg_tree = parse_regex(reg)
    reg = _norm_reg(reg_tree)
    
    while reg[0] == '(' and reg[-1] == ')' and check_balance(reg[1:-1]):
        reg = reg[1:-1]
        
    reg_tree = parse_regex(reg)
    reg = _norm_reg(reg_tree)
    
    while reg[0] == '(' and reg[-1] == ')' and check_balance(reg[1:-1]):
        reg = reg[1:-1]
    
    return reg

def solve(r):
    return norm_regex(regex_to_automata(parse_regex(r)).to_regex())

def solve_simplify(r):
    return '|'.join(simplify(norm_regex('|'.join(norm_reg(parse_regex(solve(r))))).split('|')))

r = input()
print('^' + solve_simplify(r) + '$')
