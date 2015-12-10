import _ from 'lodash';
import {List, Map, fromJS} from 'immutable';
//import {patch} from 'immutablepatch';

const verCurrent = 1;

function handleDiff(diff) {
	if (diff[0] === "=") {
		return [diff[1], diff[2]];
	}
	return null;
}

function patch1(state, user, time, id, diffs) {
	const historyData = fromJS(_(diffs).map(handleDiff).compact().zipObject());
	const historyItem = fromJS({
		data: historyData,
		time,
		user,
		ver: verCurrent
	});
	state = state
		.updateIn(
			['items', id],
			Map({data: Map(), history: List()}),
			data0 => data0
				.mergeIn(['data'], historyData)
				.mergeIn(['ver'], verCurrent)
				.update('history', List(),
					history => history.push(historyItem)
				)
		);
	return state;
}

export default function reducer(state, action) {
	try {
		switch (action.type) {
			// Snapshot format from OnTopOfThings 2014
			case 'snapshot':
				for (const item0 of action.items) {
					const historyData = _.omit(item0, 'creator', 'created', 'id');
					const data = _.omit(item0, 'id');
					data.tags = _.zipObject(_.map(item0.tags, s => [s, true]));
					const item = {
						ver: verCurrent,
						data: data,
						history: [{
							data: historyData,
							time: item0.created,
							user: item0.creator
						}]
					};
					state = state.setIn(['items', item0.id], fromJS(item));
				}
				break;
			case 'patch1': {
				state = patch1(state, action.user, action.time, action.id, action.diffs);
				break;
			}
			case 'patchN': {
				for (const hunk of action.hunks) {
					for (const id of hunk.ids) {
						state = patch1(state, action.user, action.time, id, hunk.diffs);
					}
				}
				break;
			}
		}
	} catch (e) {
		console.log("ERROR")
		console.log(e.message);
		console.log(e.stack);
	}
	return state;
}
