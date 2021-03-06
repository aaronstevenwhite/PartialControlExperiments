
var shuffleSequence = seq("consent", "intro", "p", "begin", sepWith("sep", rshuffle(rshuffle("noncollective", "collective", "filler"))), "debrief");
var practiceItemTypes = ["p"];

var defaults = [
    "Separator", {
        transfer: 500,
        hideProgressBar: true,
        normalMessage: "+"
    },
    "Message", {
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
		validators: {age: function (s) { if (s.match(/^\d+$/)) return true;
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

    ["collective", "AcceptabilityJudgment", {s: 'Betty said that Val regretted renting an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Sal said that Harriet regretted hanging out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Erin said that Sal regretted taking yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Ken said that Betty regretted participating in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Betty said that Val regretted renting an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Sal said that Harriet regretted hanging out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Erin said that Sal regretted taking yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Ken said that Betty regretted participating in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Matt said that Erin deserved to rent an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Dan said that Mel deserved to hang out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Carol said that Ken deserved to take yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Mel said that Matt deserved to participate in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Matt said that Erin deserved to rent an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Dan said that Mel deserved to hang out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Carol said that Ken deserved to take yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Mel said that Matt deserved to participate in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Carol said that Ken hated to rent an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Sal said that Harriet hated to hang out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Matt said that Erin hated to take yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jan said that Jerry hated to participate in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Carol said that Ken hated to rent an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Sal said that Harriet hated to hang out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Matt said that Erin hated to take yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jan said that Jerry hated to participate in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Carol said that Ken liked renting an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Ken said that Betty liked hanging out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Val said that Jan liked taking yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Matt said that Erin liked participating in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Carol said that Ken liked renting an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Ken said that Betty liked hanging out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Val said that Jan liked taking yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Matt said that Erin liked participating in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Erin said that Sal remembered renting an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jan said that Jerry remembered hanging out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Sal said that Harriet remembered taking yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Betty said that Val remembered participating in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Erin said that Sal remembered renting an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jan said that Jerry remembered hanging out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Sal said that Harriet remembered taking yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Betty said that Val remembered participating in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jan said that Jerry rented an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Carol said that Ken hung out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Val said that Jan took yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Sal said that Harriet participated in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jan said that Jerry rented an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Carol said that Ken hung out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Val said that Jan took yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Sal said that Harriet participated in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jan said that Jerry loved renting an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Ken said that Betty loved hanging out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Betty said that Val loved taking yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Mel said that Matt loved participating in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jan said that Jerry loved renting an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Ken said that Betty loved hanging out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Betty said that Val loved taking yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Mel said that Matt loved participating in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Erin said that Sal liked to rent an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jerry said that Carol liked to hang out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Carol said that Ken liked to take yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Matt said that Erin liked to participate in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Erin said that Sal liked to rent an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jerry said that Carol liked to hang out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Carol said that Ken liked to take yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Matt said that Erin liked to participate in the book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet and Mike remembered to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty and Fred remembered to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Erin said that Sal and Rita remembered to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val and Barbara remembered to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Carol said that Ken and Jill remembered to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jan said that Jerry and Beth remembered to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin and Bill remembered to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Mel said that Matt and Daphne remembered to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet and Mike regretted to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty and Fred regretted to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Erin said that Sal and Rita regretted to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val and Barbara regretted to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Carol said that Ken and Jill regretted to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jan said that Jerry and Beth regretted to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin and Bill regretted to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Mel said that Matt and Daphne regretted to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin deserved Bill to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Carol said that Ken deserved Jill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty deserved Fred to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan deserved Guy to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Erin said that Sal deserved Rita to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val deserved Barbara to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol deserved Hal to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet deserved Mike to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin pretended Bill to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Carol said that Ken pretended Jill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty pretended Fred to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan pretended Guy to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Erin said that Sal pretended Rita to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val pretended Barbara to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol pretended Hal to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet pretended Mike to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin liked Bill to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Carol said that Ken liked Jill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty liked Fred to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan liked Guy to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Erin said that Sal liked Rita to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val liked Barbara to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol liked Hal to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet liked Mike to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin hated Bill to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Carol said that Ken hated Jill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty hated Fred to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan hated Guy to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Erin said that Sal hated Rita to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val hated Barbara to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol hated Hal to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet hated Mike to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty loved for Fred to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin loved for Bill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan loved for Guy to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jan said that Jerry loved for Beth to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Mel said that Matt loved for Daphne to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet loved for Mike to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val loved for Barbara to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol loved for Hal to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty regretted for Fred to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin regretted for Bill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan regretted for Guy to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jan said that Jerry regretted for Beth to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Mel said that Matt regretted for Daphne to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet regretted for Mike to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val regretted for Barbara to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol regretted for Hal to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty deserved for Fred to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin deserved for Bill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan deserved for Guy to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jan said that Jerry deserved for Beth to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Mel said that Matt deserved for Daphne to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet deserved for Mike to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val deserved for Barbara to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol deserved for Hal to hang out at the mall.'}],
	["filler", "AcceptabilityJudgment", {s: 'Ken said that Betty pretended for Fred to hang out at the mall together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Matt said that Erin pretended for Bill to take yoga classes together.'}],
	["filler", "AcceptabilityJudgment", {s: 'Val said that Jan pretended for Guy to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jan said that Jerry pretended for Beth to rent an apartment in Baltimore alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Mel said that Matt pretended for Daphne to take yoga classes alone.'}],
	["filler", "AcceptabilityJudgment", {s: 'Sal said that Harriet pretended for Mike to participate in a book club.'}],
	["filler", "AcceptabilityJudgment", {s: 'Betty said that Val pretended for Barbara to rent an apartment in Baltimore.'}],
	["filler", "AcceptabilityJudgment", {s: 'Jerry said that Carol pretended for Hal to hang out at the mall.'}],
	["collective", "AcceptabilityJudgment", {s: 'Sal said that Harriet hated renting an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Mel said that Matt hated hanging out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Dan said that Mel hated taking yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Erin said that Sal hated participating in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Sal said that Harriet hated renting an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Mel said that Matt hated hanging out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Dan said that Mel hated taking yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Erin said that Sal hated participating in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Val said that Jan pretended to rent an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jerry said that Carol pretended to hang out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Mel said that Matt pretended to take yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Matt said that Erin pretended to participate in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Val said that Jan pretended to rent an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jerry said that Carol pretended to hang out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Mel said that Matt pretended to take yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Matt said that Erin pretended to participate in the book club.'}],
	["collective", "AcceptabilityJudgment", {s: 'Erin said that Sal remembered to rent an apartment in Baltimore together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Jerry said that Carol remembered to hang out at the mall together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Matt said that Erin remembered to take yoga classes together.'}],
	["collective", "AcceptabilityJudgment", {s: 'Betty said that Val remembered to participate in the book club together.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Erin said that Sal remembered to rent an apartment in Baltimore.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Jerry said that Carol remembered to hang out at the mall.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Matt said that Erin remembered to take yoga classes.'}],
	["noncollective", "AcceptabilityJudgment", {s: 'Betty said that Val remembered to participate in the book club.'}],
	[["debrief", 1], "Form", {html: { include: "debrief0.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief1.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief2.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief3.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief4.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief5.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief6.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief7.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief8.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief9.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief10.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief11.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief12.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief13.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief14.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief15.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief16.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief17.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief18.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief19.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief20.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief21.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief22.html" }}],
	[["debrief", 1], "Form", {html: { include: "debrief23.html" }}]

];
