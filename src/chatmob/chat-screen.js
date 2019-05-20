import React from 'react';
import {
	Text, View,
	Image, FlatList, TextInput, Keyboard,
	StyleSheet, Animated
} from 'react-native';

import chatService from './chat-service';

export default class ChatScreen extends React.Component {

	constructor(props) {
		super(props);

		this.state = {
			input: "",
			data: [],
			index: 1,
			messageTypes: [],
			messagePostingStatus: "Sending...",
			messageStatusStyle: styles.messageStatusVisible,
			keyboardHeight: 0
		}

	}

	componentDidMount() {
		this.keyboardWillShowListener = Keyboard.addListener(
			'keyboardWillShow',
			this.keyboardWillShow.bind(this),
		);
		this.keyboardWillHideListener = Keyboard.addListener(
			'keyboardWillHide',
			this.keyboardWillHide.bind(this),
		);
	}

	keyboardWillShow(e) {
		this.setState({
			keyboardHeight: e.endCoordinates.height
		});
	}

	keyboardWillHide() {
		this.setState({
			keyboardHeight: 0
		});
	}

	addMessage(message, messageType) {
		var o = {
			text: message,
			key: this.state.index.toString()
		};
		console.log("index A <", message, ">", this.state.index);
		this.setState({
			input: "",
			data: [...this.state.data, o],
			index: this.state.index + 1,
			messageTypes: [...this.state.messageTypes, messageType],
		});
		console.log("index B <", message, ">", this.state.index);
	}

	onTextInput(event) {
		this.addMessage(event.nativeEvent.text, "outgoingMessage");
		let self = this;
		chatService.post(event.nativeEvent.text)
		.then(function(resp) {
			console.log(resp);
			self.setState({
				messagePostingStatus: "(sent)",
			});
		})
		.catch(function(error) {
			console.log(error);
			self.setState({
				messagePostingStatus: "(error!)",
			});
		});
		this._conversationView.scrollToEnd();
	}

	textStyle(index) {
		style = styles[this.state.messageTypes[index]];
		return style;
	}

	render() {
		let pic = {
			uri: 'https://cdn.pixabay.com/photo/2019/01/16/20/52/chatbot-3936760_960_720.jpg'
		}
		return (
			<View
				style={styles.mainView}
				ref={(c) => this._mainView = c}
			>
				<Image source={pic} style={{ flex: 0, width: '100%', height: 150 }} />
				<View style = {[styles.container, {flex: 0}]}>
					<Text style={styles.heading}>Welcome to BotAgg Chat!</Text>
					<View style={styles.title}><Text>Conversation</Text></View>
				</View>
				<View style={{flex: 1, paddingBottom: this.state.keyboardHeight}}>
					<View style = {[styles.container, {flex: 1}]}>
						<FlatList
							ref={(c) => this._conversationView = c}
							data={this.state.data}
							style={styles.list}
							renderItem={({ item, index }) =>
								<View style={styles.messageRow}>
									<View
										style={styles.message}
										accessibilityLabel={item.text}
									>
										<Text
											style={this.textStyle(index)}
										>
											{item.text}
										</Text>
									</View>
									<View 
										style={this.state.messageStatusStyle}
									>
										<Text 
											style={styles.messageStatusText}
											testID="messageStatus"
											>{this.state.messagePostingStatus}</Text>
									</View>
								</View>
							}
						/>
					</View>
					<View style = {[styles.container, {flex: 0}]}>
						<TextInput
							testID="messageText"
							accessibilityLabel="message input field"
							ref={(c) => this._textInput = c}
							value={this.state.input}
							style={styles.input}
							placeholder="Type your message"
							onChangeText={(text) => this.setState({ input: text })}
							onSubmitEditing={(event) => this.onTextInput(event)}
						></TextInput>
					</View>
				</View>
			</View>
		);
	}
};


const styles = StyleSheet.create({
	mainView: {
		backgroundColor: '#eee',
		flex: 1
	},
	container: {
		alignItems: 'stretch',
		justifyContent: 'flex-start',
		backgroundColor: '#fff',
		margin: 5
	},
	title: {
		height: 20,
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
		margin: 5
	},
	messageStatusVisible: {
	},
	heading: {
		height: 30,
		color: 'black',
		fontSize: 18
	},
	subtext: {
		height: 10,
		color: 'gray',
		fontSize: 9
	},
	list: { 
		margin: 5, 
		flex: 1, 
		alignSelf: 'stretch' 
	},
	input: {
		height: 24,
		borderWidth: StyleSheet.hairlineWidth,
		borderRadius: 5
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

