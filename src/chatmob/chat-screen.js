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
		}

		this.expandedPicHeight = 150;
		this.minimizedPicHeight = 80;
		this.keyboardHeight = new Animated.Value(0);
		this.picHeight = new Animated.Value(this.expandedPicHeight);
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
		this.keyboardDidShowListener = Keyboard.addListener(
			'keyboardDidShow',
			this.keyboardDidShow.bind(this),
		);
		this.keyboardDidHideListener = Keyboard.addListener(
			'keyboardDidHide',
			this.keyboardDidHide.bind(this),
		);
	}

	keyboardWillShow(e) {
		let showDuration = e.duration * 0.60;
		Animated.parallel([
			Animated.timing(this.keyboardHeight, {
				duration: showDuration,
				toValue: e.endCoordinates.height
			}),
			Animated.timing(this.picHeight, {
				duration: showDuration,
				toValue: this.minimizedPicHeight
			})
		]).start();
	}

	keyboardDidShow(e) {
	}

	keyboardWillHide(e) {
		Animated.parallel([
			Animated.timing(this.keyboardHeight, {
				duration: e.duration,
				toValue: 0
			}),
			Animated.timing(this.picHeight, {
				duration: e.duration,
				toValue: this.expandedPicHeight
			})
		]).start();
	}

	keyboardDidHide(e) {
		//this._conversationView.scrollToEnd({animated: false});
	}

	addMessage(message, messageType) {
		var o = {
			text: message,
			key: this.state.index.toString()
		};
		this.setState({
			input: "",
			data: [o, ...this.state.data],
			index: this.state.index + 1,
			messageTypes: [...this.state.messageTypes, messageType],
		});
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
	}

	showLastMessage(options = {flash: true}) {
		this._conversationView.scrollToOffset({x:0, y: 0, animated: false});
		if (options == null) {
			return;
		}
		if (options.flash == true) {
			this._conversationView.flashScrollIndicators();
		}
	}

	onConversationChanged(contentWidth, contentHeight) {
		this.showLastMessage();
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
				<Animated.Image source={pic} style={{ flex: 0, width: '100%', height: this.picHeight }} />
				<View><Text style={[styles.title, {flex: 0}]}>Conversation</Text></View>
				<Animated.View style={{flex: 1, paddingBottom: this.keyboardHeight}}>
					<View 
						style={[styles.container, {flex: 1}]} 
					>
						<FlatList
							testID="conversationList"
							ref={(c) => this._conversationView = c}
							data={this.state.data}
							style={styles.list}
							inverted={true}
							onContentSizeChange={(contentWidth, contentHeight) => this.onConversationChanged(contentWidth, contentHeight)}
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
					<View style={[styles.container, {flex: 0}]}>
						<TextInput
							testID="messageText"
							accessibilityLabel="message input field"
							ref={(c) => this._textInput = c}
							value={this.state.input}
							style={styles.input}
							blurOnSubmit={false}
							placeholder="Type your message"
							onChangeText={(text) => this.setState({ input: text })}
							onSubmitEditing={(event) => this.onTextInput(event)}
						></TextInput>
					</View>
				</Animated.View>
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
		height: 16,
		color: 'black',
		fontSize: 14,
		alignItems: 'center',
		justifyContent: 'flex-end',
		borderBottomWidth: 1,
		margin: 5
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
	subtext: {
		height: 10,
		color: 'gray',
		fontSize: 9
	},
	list: { 
		margin: 5, 
		alignSelf: 'stretch' 
	},
	input: {
		height: 24,
		borderWidth: StyleSheet.hairlineWidth,
		borderRadius: 5,
		padding: 3
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

