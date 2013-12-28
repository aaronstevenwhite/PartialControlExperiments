from random import shuffle
from string import Template

verbs = ['claimed',
         'tried',
         'managed',
         'began',
         'needed',
         'wanted',
         'intended',
         'hoped',
         'was likely',
         'loved']

noncollective = [(['be', 'was'], 'happy'),
                 (['take', 'took'], 'yoga classes'),
                 (['rent', 'rented'], 'an apartment in Baltimore'),
                 (['support', 'supported'], 'the child')]

collective = [(tense_root, pred + ' together') for tense_root, pred in noncollective]

names = [('Sal', 'Harriet', 'Mike'),
         ('Val', 'Jan', 'Guy'),
         ('Jan', 'Jerry', 'Beth'),
         ('Carol', 'Ken', 'Jill'),
         ('Betty', 'Val', 'Barbara'),
         ('Mat', 'Erin', 'Bill'),
         ('Jerry', 'Carol', 'Hal'),
         ('Ken', 'Betty', 'Fred'),
         ('Erin', 'Sal', 'Rita'),
         ('Mel', 'Mat', 'Daphne'),
         ('Dan', 'Mel', 'Larry')]

sentences = {}

pred_types = {'collective'    : collective,
              'noncollective' : noncollective}

## create baseline sentences

base_temp = lambda name, pred: '{0} said that {1} {2} {3}.'.format(name[0], name[1], pred[0][1], pred[1])

sentences['base'] = {}

for ptype in pred_types:
    sentences['base'][ptype] = {pred[0][0] : [base_temp(name, pred)] for name, pred in zip(names, pred_types[ptype])}

## create test item template

cont_temp = lambda verb, name, pred: '{0} said that {1} {2} to {3} {4}.'.format(name[0], name[1], verb, pred[0][0], pred[1])

## add test to sentences

for verb in verbs:
    shuffle(names)
    sentences[verb] = {}
    for ptype in pred_types:
        sentences[verb][ptype] = {}
        for name, pred in zip(names, pred_types[ptype]):
            sentences[verb][ptype][pred[0][0]] = [cont_temp(verb, name, pred)]

## create filler item template

plr_temp = lambda verb, name, pred: '{0} said that {1} and {2} {3} to {4}.'.format(name[0], name[1], name[2], verb, pred)
ecm_temp = lambda verb, name, pred: '{0} said that {1} {2} {3} to {4}.'.format(name[0], name[1], verb, name[2], pred)
for_temp = lambda verb, name, pred: '{0} said that {1} {2} for {3} to {4}.'.format(name[0], name[1], verb, name[2], pred)

plr_verbs = ['began',
             'was likely']
ecm_verbs = ['needed',
             'wanted',
             'intended',
             'managed']
for_verbs = ['tried',
             'claimed',
             'hoped',
             'loved']

filler_preds = ['support the child together',
                'take yoga classes together',
                'be happy alone',
                'rent an apartment in Baltimore',
                'take yoga classes alone',
                'be happy',
                'rent an apartment in Baltimore',
                'support the child']

## add fillers to sentences

sentences['filler'] = {'filler' : {'filler' : []}}

for temp, verbs in [(plr_temp, plr_verbs), (ecm_temp, ecm_verbs), (for_temp, for_verbs)]:
    shuffle(names)
    for verb in verbs:
        for i, pred in enumerate(filler_preds):
            sentences['filler']['filler']['filler'].append(temp(verb, names[i], pred))


conf_temp = lambda items: Template('''
var shuffleSequence = seq("consent", "intro", "p", "begin", sepWith("sep", rshuffle(rshuffle("noncollective", "collective", "filler"))), "debrief");
var practiceItemTypes = ["p"];

var defaults = [
    "Separator", {
        transfer: 500,
        hideProgressBar: true,
        normalMessage: "+"
    },
    "Message", {
        transfer: "keypress",
        hideProgressBar: true
    },
    "AcceptabilityJudgment", {
        q: '',
        as: ["1", "2", "3", "4", "5", "6", "7"],
        presentAsScale: true,
        instructions: "Use number keys or click boxes to answer.",
        leftComment: "(Bad)", rightComment: "(Good)"
    },
    "Form", { hideProgressBar: true }

];

var items = [
	["consent", "Form", {
        html: { include: "consent.html" },
		validators: {age: function (s) { if (s.match(/^\d+$$/)) return true;
							else return "Bad value for age"; }}
    } ],

	["intro", "Message", {html: { include: "intro.html" }}],

	["p", "AcceptabilityJudgment", {s: "Gary ran that he ate a piece of cake."}],
	["p", "AcceptabilityJudgment", {s: "Stacy built a house for him."}],
	["p", "AcceptabilityJudgment", {s: "I failed going to the store."}],

	["begin", "Message", {
				html: { include: "begin.html" },
				} ],

    ["sep", "Separator", { }],

    $items

];
''').substitute(items=items)

item_temp = lambda typ, sent: Template('["$typ", "AcceptabilityJudgment", {s: \'$sent\'}]').substitute(typ=typ, sent=sent)

items = []
items_conf = []

for verb in sentences:
    for ptype in sentences[verb]:
        for pred in sentences[verb][ptype]:
            for sent in sentences[verb][ptype][pred]:
                verb = '_'.join(verb.split())
                items_conf.append('\t'.join([verb, ptype, pred]))
                items.append(item_temp(ptype, sent))

for i in range(24):
    items.append(Template('[["debrief", 1], "Form", {html: { include: "debrief${num}.html" }}]').substitute(num=str(i)))

f = open('pc_items.js', 'w')
g = open('pc_items.conf', 'w')

f.write(conf_temp(',\n\t'.join(items)))
g.write('\n'.join(items_conf))

f.close()
g.close()
