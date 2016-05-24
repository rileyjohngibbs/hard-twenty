from selenium import webdriver
import getpass, time

PATH_TO_CHROMEDRIVER = './chromedriver'

email = raw_input('Email: ')
password = getpass.getpass('Password: ')

chrome = webdriver.Chrome(PATH_TO_CHROMEDRIVER)

chrome.get('http://www.facebook.com')
chrome.find_element_by_id('email').send_keys(email)
chrome.find_element_by_id('pass').send_keys(password)
chrome.find_element_by_id('loginbutton').click()

while True:
	if 'https://www.facebook.com/pokes' not in chrome.current_url:
		chrome.get('https://www.facebook.com/pokes/')
	# Handle the screen overlay when Chrome asks for permission for notifications.
	uilayers = chrome.find_elements_by_class_name('uiLayer')
	if len(uilayers) > 1:
		uilayers[0].click()
	buttons = chrome.find_elements_by_link_text('Poke Back')
	if buttons:
		try:
			buttons[0].click()
			print 'Poked someone.'
		except:
			print 'Tried poking but missed.'
	time.sleep(3)