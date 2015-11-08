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
			Map(),
			data0 => data0.merge(historyData).update('history', List(),
				history => history.push(historyItem)
			)
		);
}

export default function reducer(state, action) {
	try {
		switch (action.type) {
			case 'snapshot':
				for (const item of action.items) {
					const historyData = _.omit(item, 'creator', 'created', 'id');
					const data = _.omit(item, 'id');
					data.tags = _.zipObject(_.map(item.tags, s => [s, true]));
					data.history = [{
						data: historyData,
						time: item.created,
						user: item.creator,
						ver: verCurrent
					}];
					data.ver = verCurrent;
					state = state.setIn(['items', item.id], fromJS(data));
				}
				break;
			case 'patch1': {
				patch1(state, action.user, action.time, action.id, action.diffs);
				break;
			}
			case 'patchN': {
				for (const hunk of action.hunks) {
					for (const id of hunk.ids) {
						patch1(state, action.user, action.time, id, hunk.diffs);
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
