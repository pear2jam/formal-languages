class ENfaToNfaConverter:
    def __init__(self):
        self.enfa_transition_table = None
        self.nfa_transition_table = None
        self.inputs = None
        self.states = None
        self.start_state = None
        self.final_states = None
        self.method = None

    def convert(self, table, alpha, states, start, finishes):
        self.enfa_transition_table = table
        self.inputs = alpha
        self.states = states
        self.start_state = start
        self.final_states = finishes
        self.nfa_transition_table = None

        for state in self.states:
            self.enfa_transition_table[state]['CL(q)'] = self._get_closure_states(state)
            for i in self.inputs:
                if i != 'ε':
                    self.enfa_transition_table[state]['S' + str(i)] = self._getS(i, state)

        for state in self.states:
            for i in self.inputs:
                if i != 'ε':
                    si_closure = []
                    si_states = self.enfa_transition_table[state]['S' + str(i)]
                    for si_state in si_states:
                        si_closure += self._get_closure_states(si_state)
                    self.enfa_transition_table[state]['δN(q,' + str(i) + ')'] = set(si_closure)

    def _get_closure_states(self, state):
        closure_states = set()
        states_stack = [state]

        while (len(states_stack)):
            curr_state = states_stack.pop(0)
            if curr_state not in closure_states:
                closure_states.add(curr_state)
            for i in self.enfa_transition_table[curr_state]['ε']:
                if i not in closure_states:
                    states_stack.append(i)

        return closure_states

    def _getS(self, input, state):
        closure_states = self.enfa_transition_table[state]['CL(q)']
        s = []
        for state in closure_states:
            s += self.enfa_transition_table[state][input]
        return set(s)


class NfaToDfaConverter:
    def __init__(self):
        self.dfa_transition_table = None
        self.nfa_transition_table = None
        self.inputs = None
        self.states = None
        self.start_state = None
        self.final_states = None
        self.method = None

    def convert(self, table, alpha, states, start, finishes, method='2'):
        self.nfa_transition_table = table
        self.inputs = alpha
        self.states = states
        self.start_state = start
        self.final_states = finishes

        
        self._lazy_determine()

    def _lazy_determine(self):
        self.dfa_transition_table = {}
        new_states = [{self.start_state}]
        created_states = []

        while len(new_states) > 0:
            curr_state = new_states.pop(0)
            self.dfa_transition_table[frozenset(curr_state)] = {}
            created_states.append(curr_state)
            for i in self.inputs:
                output_states = []
                for state in curr_state:
                    output_states += self.nfa_transition_table[state][i]
                output_states = set(sorted(output_states))
                if output_states not in created_states:
                    new_states.append(output_states)
                self.dfa_transition_table[frozenset(curr_state)][i] = output_states
