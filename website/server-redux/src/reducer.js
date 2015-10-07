import _ from 'lodash';
import {List, Map, fromJS} from 'immutable';
//import {patch} from 'immutablepatch';

function handleDiff(diff) {
	if (diff[0] === "=") {
		return [diff[1], diff[2]];
	}
	return null;
}

function patch1(state, user, time, id, diffs) {
	const historyData = fromJS(_(diffs).map(handleDiff).compact().zipObject());
	const historyItem = fromJS({
		user,
		time,
		data: historyData
	});
	state = state
		.updateIn(
			['items', id, 'history'],
			List(),
			history => history.push(historyItem)
		)
		.updateIn(
			['items', id, 'data'],
			Map(),
			data0 => data0.merge(historyData)
		);
}

export default function reducer(state, action) {
	switch (action.type) {
		case 'snapshot':
			for (const item of action.items) {
				state = state.setIn(['items', item.id], fromJS({
					id: item.id,
					history: [{
						user: item.creator,
						time: item.created,
						data: item
					}],
					data: item
				}));
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
	return state;
}

export function reducerOld(state, action) {
	switch (action.type) {
		case 'snapshot':
			for (const item of action.items) {
				state = state.setIn(['items', item.id], fromJS(item));
			}
			break;
		case 'patch1': {
			const id = action.id;
			for (const diff of action.diffs) {
				if (diff[0] === "=") {
					const name = diff[1];
					state = state.setIn(['items', id, name], diff[2]);
				}
			}
			break;
		}
		case 'patchN': {
			for (const hunk of action.hunks) {
				for (const id of hunk.ids) {
					for (const diff of hunk.diffs) {
						if (diff[0] === "=") {
							var name = diff[1];
							state = state.setIn(['items', id, name], diff[2]);
						}
					}
				}
			}
			break;
		}
	}
	return state;
}
