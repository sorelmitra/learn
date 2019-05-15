import { StyleSheet } from 'react-native';

export const heights = {
	logo: 150,
	head: 30,
	subtext: 10,
	input: 15,
	conv: 20,
	padding: 10
};

export const styles = StyleSheet.create({
	container: {
	  flex: 0,
	  alignItems: 'stretch',
	  justifyContent: 'flex-start',
	  backgroundColor: '#fff'      
	},
	title: {
	  height: heights.conv,
	  color: 'black',
	  fontSize: 14,
	  justifyContent: 'flex-end'
	},
	messageRow: {
		flexDirection: 'row'
	},
	message: {
	  height: 30,
	  justifyContent: 'center'
	},
	messageStatusText: {
		fontSize: 10,
		color: 'gray',
		padding: 5
	},
	messageStatusVisible: {
	},
	heading: {
	  height: heights.head,
	  color: 'black',
	  fontSize: 18
	},
	subtext: {
	  height: heights.subtext,
	  color: 'gray',
	  fontSize: 9
	},
	input: {
	  height: heights.input
	},
	outgoingMessage: {
	  color: 'green',
	  fontSize: 14
	},
	incomingMessage: {
	  color: 'red',
	  fontSize: 14,
	  textAlign: 'right'
	}
  });
  