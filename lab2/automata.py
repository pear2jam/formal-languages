class Automata:
    def __init__(self, states, alphabet, transitions, start, finish):
        self.states = states  # set of numbers - states
        self.alphabet = alphabet  # set of symbols
        self.transitions = transitions  # dict: [[symbol, state]+]
        self.start = start  # number
        self.finish = finish  # number

        self.transitions_matrix = None

    @staticmethod
    def map_transitions(d: dict, map_d: dict):
        """
        d : словарь переходов, номера состояний которого нужно изменить согласно словарю map_d
        map_d : словарь изменения номеров состояний на новые

        return : новый словарь переходов автомата
        """
        d_new = dict()
        for key in d.keys():
            d_new[map_d[key]] = [[a[0], map_d[a[1]]] for a in d[key]]
        return d_new

    @staticmethod
    def union_dicts(d1, d2: dict):
        """
        d1, d2 : словари переходов автомата
        return : словарь объедения переходов d1 d2
        """
        d_new = dict(d1)
        for key in d2.keys():
            if key in d_new:
                d_new[key] = list(d2[key] + (d_new[key]))
            else:
                d_new[key] = d2[key]

        return d_new

    @staticmethod
    def list_transitions(d: dict):
        """
        Разворачивает словарь переходов в список вида [state1, symbol, state2]
        return list(list()*)
        """
        res = []
        for key, value in d.items():
            res += [[key, a[0], a[1]] for a in value]
        return res

    @staticmethod
    def dict_transitions(l: list):
        """
        Упаковывает список переходов вида [state1, symbol, state2] в словарь
        return dict
        """
        res = {}
        for i in l:
            if i[0] not in res: res[i[0]] = []
            res[i[0]].append([i[1], i[2]])
        return res

    @staticmethod
    def unify_transitions(d: dict):
        unify_list = lambda l: list(list(i) for i in set(tuple(j) for j in l))

        for key, value in d.items(): d[key] = unify_list(d[key])
        return d

    def set_transition_matrix(self):
        """
        Считает и присваивает матрицу переходов объекту автомата
        """
        matrix = [['NONE' for i in range(len(self.states))] for j in range(len(self.states))]
        for t in self.list_transitions(self.transitions):
            if matrix[t[0]][t[2]] == 'NONE':
                matrix[t[0]][t[2]] = t[1]
            else:
                matrix[t[0]][t[2]] += ('|' + t[1])
        self.transitions_matrix = matrix

    def to_regex(self):
        """
        Считает методом удалений состояний и возвращает регулярное выражение эквивалентное автомату
        """

        def re_op(r1, r2, op):
            # Арифметика (*, |) регулярных выражений

            if r1 == 'NONE': return r2
            if r2 == 'NONE': return r1
            if op == '':
                if r1 == 'ε' and r2 == 'ε': return 'ε'
                if r1 == 'ε': return r2
                if r2 == 'ε': return r1
                if '|' in r1: r1 = '(' + r1 + ')'
                if '|' in r2: r2 = '(' + r2 + ')'

            return r1 + op + r2

        def re_iter(r):
            # Правильный формат итерации
            if r == 'ε' or r == 'NONE': return r
            if len(r) == 1: return r + '*'
            return '(' + r + ')*'

        new_start = Automata({0, 1}, {'ε'}, {0: [['ε', 1]]}, 0, 1)
        new_finish = Automata({0, 1}, {'ε'}, {0: [['ε', 1]]}, 0, 1)
        intermediate = Automata(self.states, self.alphabet, self.transitions, self.start, self.finish)
        aut = new_start.concat(intermediate).concat(new_finish)
        aut.set_transition_matrix()

        trans = aut.transitions_matrix
        inter_states = aut.states.difference(set([aut.finish, aut.start]))
        # print(trans, inter_states, aut)

        for state in inter_states:
            for i in aut.states:
                if i == state: continue

                # Новые переходы вершин в себя
                if trans[i][state] != 'NONE' and trans[state][i] != 'NONE':
                    trans[i][i] = re_op(trans[i][i],
                                        re_op(re_op(trans[i][state], re_iter(trans[state][state]), ''), trans[state][i],
                                              ''), '|')

                for j in aut.states:
                    if i == state or j == state or i <= j:
                        continue

                    # Новые переходы пар вершин

                    if trans[i][state] != 'NONE' and trans[state][j] != 'NONE':
                        trans[i][j] = re_op(trans[i][j], re_op(re_op(trans[i][state], re_iter(trans[state][state]), ''),
                                                               trans[state][j], ''), '|')

                    if trans[state][i] != 'NONE' and trans[j][state] != 'NONE':
                        trans[j][i] = re_op(trans[j][i], re_op(re_op(trans[j][state], re_iter(trans[state][state]), ''),
                                                               trans[state][i], ''), '|')

            # Удаляем вершину
            for i in aut.states:
                trans[state][i] = 'NONE'
                trans[i][state] = 'NONE'

        return trans[aut.start][aut.finish]

    def _concat(self, a):
        states1, states2 = list(self.states), list(a.states)
        alpha1, alpha2 = self.alphabet, a.alphabet
        trans1, trans2 = self.transitions, a.transitions
        start1, start2 = self.start, a.start
        finish1, finish2 = self.finish, a.finish
        states_new = list(range(len(states1) + len(states2) - 1))  # -1 так как одно start2 становится finish1
        states2.remove(start2)
        states_new_mapping = dict(
            zip(states2 + [start2], list(range(len(states1), len(states1) + len(states2))) + [finish1]))
        alphabet_new = alpha1.union(alpha2)
        transitions_new = self.union_dicts(dict(trans1), self.map_transitions(trans2, states_new_mapping))
        return Automata(set(states_new), alphabet_new, self.unify_transitions(transitions_new), start1,
                        states_new_mapping[finish2])

    def concat(self, a):
        start, finish, trans = self.start, self.finish, self.transitions
        if start in trans and ['ε', finish] in trans[start]:
            return self._concat(a).parallel(a)
        else:
            return self._concat(a)

    def parallel(self, a):
        states1, states2 = list(self.states), list(a.states)
        alpha1, alpha2 = self.alphabet, a.alphabet
        trans1, trans2 = self.transitions, a.transitions
        start1, start2 = self.start, a.start
        finish1, finish2 = self.finish, a.finish

        states2 = list(set(states2).difference({start2, finish2}))
        states_new = list(range(len(states1) + len(states2)))  # -1 так как одно start2 становится finish1
        # states2.remove(start2)
        # states2.remove(finish2)
        states_new_mapping = dict(zip(states2 + [start2] + [finish2],
                                      list(range(len(states1), len(states1) + len(states2))) + [start1] + [finish1]))
        alphabet_new = alpha1.union(alpha2)
        transitions_new = self.union_dicts(dict(trans1), self.map_transitions(trans2, states_new_mapping))
        return Automata(set(states_new), alphabet_new, self.unify_transitions(transitions_new), start1,
                        states_new_mapping[finish2])

    def product(self, a):
        states1, states2 = list(self.states), list(a.states)
        alpha1, alpha2 = self.alphabet, a.alphabet
        trans1, trans2 = self.transitions, a.transitions
        start1, start2 = self.start, a.start
        finish1, finish2 = self.finish, a.finish

        alphabet_new = alpha1.union(alpha2)
        """
        Взяли два перехода в двух автоматах:
        если у этих переходов одна буква, то для этих прямого произведения этих состояний вводится переход по этой букве 
        (0 (a) -> 1; 2 (a) -> 3 ==> [0 2] (a) -> [1 3])
        """

        trans1_list, trans2_list = self.list_transitions(trans1), self.list_transitions(trans2)

        transitions_pairs = []
        state_pairs = []

        for tr_i in trans1_list:
            for tr_j in trans2_list:
                if tr_i[1] == tr_j[1]:
                    if str(tr_i[0]) + '&' + str(tr_j[0]) not in state_pairs: state_pairs.append(
                        str(tr_i[0]) + '&' + str(tr_j[0]))
                    if str(tr_i[2]) + '&' + str(tr_j[2]) not in state_pairs: state_pairs.append(
                        str(tr_i[2]) + '&' + str(tr_j[2]))
                    transitions_pairs.append(
                        [str(tr_i[0]) + '&' + str(tr_j[0]), tr_i[1], str(tr_i[2]) + '&' + str(tr_j[2])])

        state_pairs.append(str(start1) + '&' + str(start2))
        state_pairs.append(str(finish1) + '&' + str(finish2))

        state_pairs = list(set(state_pairs))

        states_mapping = dict(zip(state_pairs, list(range(len(state_pairs)))))

        start_new = states_mapping[str(start1) + '&' + str(start2)]
        finish_new = states_mapping[str(finish1) + '&' + str(finish2)]

        transitions_new = self.map_transitions(self.dict_transitions(transitions_pairs), states_mapping)

        return Automata(set(range(len(state_pairs))), alphabet_new, self.unify_transitions(transitions_new), start_new,
                        finish_new)

    def iteration(self):
        # print(self.states)
        mapping = dict(zip(list(range(len(self.states))), list(range(len(self.states)))))
        # print(mapping)
        mapping[self.finish] = self.start
        # print(mapping)
        states_new = set(self.states)
        states_new.remove(self.finish)
        eps_aut = Automata({0, 1}, {'ε'}, {0: [['ε', 1]]}, 0, 1)
        res = eps_aut.concat(
            Automata(states_new, self.alphabet, self.unify_transitions(self.map_transitions(self.transitions, mapping)),
                     self.start, self.start)).concat(eps_aut)
        return res.parallel(eps_aut)

    def __str__(self):
        def print_transition(t):
            return '\n\t     '.join([f'{t[0]} ({i[0]}) -> {i[1]}' for i in t[1]])

        transitions_str = '\n\t     '.join([print_transition(i) for i in self.transitions.items()])
        return f'States: {self.states}\nAlphabet: {self.alphabet}\nTransitions: {transitions_str}\
        \nStart: {self.start}\nFinish: {self.finish}'
